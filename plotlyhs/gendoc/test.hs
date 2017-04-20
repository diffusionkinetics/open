{-# LANGUAGE OverloadedStrings #-}

import Lucid
import Lucid.Html5
import Graphics.Plotly
import Graphics.Plotly.Lucid
import Lens.Micro

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

main =
    T.writeFile "test.html" $ renderText $ doctypehtml_ $ do
    head_ $ do meta_ [charset_ "utf-8"]
               plotlyCDN
    body_ $ toHtml $ plotly "myDiv" [myTrace]

pointsData :: [(Double, Double)]
pointsData = zip [1,2,3,4] [500,3000,700,200]

myTrace
  = line (aes & x .~ fst
              & y .~ snd) pointsData