{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

{-|

Plot traces to html using blaze-html

Example code:

@
plotHtml :: Html ()
plotHtml = toHtml $ plotly "myDiv" [trace] & layout . title ?~ "my plot"
                                           & layout . width ?~ 300

@

where `trace` is a value of type `Trace`

-}
module Graphics.Plotly.Blaze where

import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Graphics.Plotly
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson

-- |`script` tag to go in the header to import the plotly.js javascript from the official CDN
plotlyCDN :: H.Html
plotlyCDN = H.script ! A.src "https://cdn.plot.ly/plotly-latest.min.js" $ ""

-- |Activate a plot defined by a `Plotly` value
plotlyJS :: Plotly -> H.Html
plotlyJS (Plotly divNm trs lay) =
  let trJSON = decodeUtf8 $ toStrict $ encode trs
      layoutJSON = decodeUtf8 $ toStrict $ encode lay
  in H.script $ H.toHtml ("Plotly.newPlot('"<>divNm<>"', "<>trJSON<>","<>layoutJSON<>", {displayModeBar: false});")

-- |Create a div for a Plotly value
plotlyDiv :: Plotly -> H.Html
plotlyDiv (Plotly divNm _ _) =
  H.div ! A.id (toValue divNm) $ ""


instance ToMarkup Plotly where
  toMarkup pl = plotlyDiv pl >> plotlyJS pl
