{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}


{-|

Plot traces to html

Example code:

@
plotHtml :: Html ()
plotHtml = do div_ [id_ "myDiv", style_ "width: 480px; height: 400px;"] ""
              newPlot "myDiv" $ plotly [trace] & layout . title ?~  "my plot"
@

where `trace` is a value of type `Trace`

-}
module Graphics.Plotly.Lucid where

import Lucid
import Graphics.Plotly
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson
import Data.Text (Text)

-- |`script` tag to go in the header to import the plotly.js javascript from the official CDN
plotlyCDN :: Monad m => HtmlT m ()
plotlyCDN = script_ [src_ "https://cdn.plot.ly/plotly-latest.min.js"] ""

-- |Activate a plot defined by a `Plotly` value in a given `div` (which is not created).
plotlyJS :: Monad m => Plotly -> HtmlT m ()
plotlyJS (Plotly divNm trs lay) =
  let trJSON = decodeUtf8 $ toStrict $ encode trs
      layoutJSON = decodeUtf8 $ toStrict $ encode lay
  in script_ ("Plotly.newPlot('"<>divNm<>"', "<>trJSON<>","<>layoutJSON<>", {displayModeBar: false});")

plotlyDiv :: Monad m => Plotly -> HtmlT m ()
plotlyDiv (Plotly divNm _ mlay) =
  div_ [id_ divNm]
--        style_ "width: 500px; height: 500px;"]
       ""

instance ToHtml Plotly where
  toHtml pl = plotlyDiv pl >> plotlyJS pl
