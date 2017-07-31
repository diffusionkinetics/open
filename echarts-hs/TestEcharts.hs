{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Lucid
import Lucid.Bootstrap3
import Graphics.Echarts

main :: IO ()
main = do
  T.writeFile "echartstest.html" $ renderText testpage

testpage =  doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    cdnJqueryJS
    cdnBootstrapJS
    echartsCDN

  body_ $ do
     "Hello World"
     div_ [style_ "width: 600px; height: 600px", id_ "main"] ""
     script_ $ runEcharts element "graph" nodes edges

element = "main"

nodes = [Data "node1" 300 300, Data "node2" 800 300]

edges = [Link "node1" "node2"]
