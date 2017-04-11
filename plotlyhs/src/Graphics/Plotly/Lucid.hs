{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

{-|

Plot traces to html using lucid

Example code:

@
plotHtml :: Html ()
plotHtml = toHtml $ plotly "myDiv" [trace] & layout . title ?~ "my plot"
                                           & layout . width ?~ 300

@

where `trace` is a value of type `Trace`

-}
module Graphics.Plotly.Lucid where

import Lucid
import Graphics.Plotly.Base
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson

-- |`script` tag to go in the header to import the plotly.js javascript from the official CDN
plotlyCDN :: Monad m => HtmlT m ()
plotlyCDN = script_ [src_ "https://cdn.plot.ly/plotly-latest.min.js"] ""

-- |Activate a plot defined by a `Plotly` value
plotlyJS :: Monad m => Plotly -> HtmlT m ()
plotlyJS (Plotly divNm trs lay) =
  let trJSON = decodeUtf8 $ toStrict $ encode trs
      layoutJSON = decodeUtf8 $ toStrict $ encode lay
  in script_ ("Plotly.newPlot('"<>divNm<>"', "<>trJSON<>","<>layoutJSON<>", {displayModeBar: false});")

-- |Create a div for a Plotly value
plotlyDiv :: Monad m => Plotly -> HtmlT m ()
plotlyDiv (Plotly divNm _ _) =
  div_ [id_ divNm]
       ""

instance ToHtml Plotly where
  toHtml pl = plotlyDiv pl >> plotlyJS pl
  toHtmlRaw = toHtml
