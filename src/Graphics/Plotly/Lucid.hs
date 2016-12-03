{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Graphics.Plotly.Lucid where

import Lucid
import Graphics.Plotly
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson
import Data.Text (pack, Text)

plotlyCDN :: Html ()
plotlyCDN = script_ [src_ "https://cdn.plot.ly/plotly-latest.min.js"] ""

newPlot :: Text -> Plotly -> Html ()
newPlot divNm (Plotly trs lay) =
  let trJSON = decodeUtf8 $ toStrict $ encode trs
      layoutJSON = {-case mlay of
                     Nothing -> ""
                     Just lay -> -} ","<>(decodeUtf8 $ toStrict $ encode lay)
  in script_ ("Plotly.newPlot('"<>divNm<>"', "<>trJSON<>layoutJSON<>", {displayModeBar: false});")
