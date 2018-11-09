{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Lucid
import Lucid.Html5
import Graphics.Plotly
import Graphics.Plotly.Lucid
import Graphics.Plotly.GoG
import Lens.Micro

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

-- We'd like to generate
--
-- <script>Plotly.newPlot('myContour', [{"z":[[10, 10.625, 12.5, 15.625, 20],
--                                            [5.625, 6.25, 8.125, 11.25, 15.625],
--                                            [2.5, 3.125, 5.0, 8.125, 12.5],
--                                            [0.625, 1.25, 3.125, 6.25, 10.625],
--                                            [0, 0.625, 2.5, 5.625, 10]],
--                                        "x":[-9, -6, -5 , -3, -1],
--                                        "y":[0, 1, 4, 5, 7],"type":"contour"}],
--                         {}, {displayModeBar: false});
-- </script>

zss :: [[Double]]
zss = [[10, 10.625, 12.5, 15.625, 20],
       [5.625, 6.25, 8.125, 11.25, 15.625],
       [2.5, 3.125, 5.0, 8.125, 12.5],
       [0.625, 1.25, 3.125, 6.25, 10.625],
       [0, 0.625, 2.5, 5.625, 10]]

xs, ys :: [Double]
xs = [-9, -6, -5 , -3, -1]
ys = [0, 1, 4, 5, 7]

bar = hcontour (aes & x.~ fst & y .~ (fst. snd) & z .~ (snd . snd))
               (zip xs (zip ys zss))

main =
  T.writeFile "contour.html" $ renderText $ doctypehtml_ $ do
    head_ $ do meta_ [charset_ "utf-8"]
               plotlyCDN
    body_ $ toHtml $ plotly "myContour" [bar]

