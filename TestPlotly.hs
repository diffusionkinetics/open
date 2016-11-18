{-# LANGUAGE OverloadedStrings,ExtendedDefaultRules #-}

import Lucid
import Lucid.Html5
import Graphics.Plotly

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

main = T.putStrLn $ renderText $ doctypehtml_ $ do
  head_ $ do meta_ [charset_ "utf-8"]
             script_ [src_ "https://cdn.plot.ly/plotly-latest.min.js"] ""
--  head_ $ script_ "hello"
  body_ $ do div_ [id_ "myDiv", style_ "width: 480px; height: 400px;"] ""
             newPlot "myDiv" [myTrace] defLayout { title = Just "my plot" }
             p_ "hello world"

myTrace = scatter { x = Just [1,2,3,4], y = Just [5,3,7,2], mode = Just [Markers] }
