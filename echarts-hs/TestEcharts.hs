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
     script_ $ runEcharts element options

element = "main"

options = EchartsOptions defTooltip
          $ Series "graph" 50 True (NormalLabel $ NormalLabelData (Just True) Nothing)
            ["circle","arrow"] [4,10] (NormalLabel $ NormalLabelData (Just True) (Just $ TextStyle 20)) (NormalLineStyle $ NormalLineStyleData 2 0 0.9) nodes edges

nodes = [Data "node1" 300 300, Data "node2" 800 300,
         Data "node3" 550 100, Data "node4" 550 500]

edges = [Link "node1" "node2" $ Just $ NormalLineStyle $ NormalLineStyleData 5 0.2 0.5,
         Link "node2" "node1" $ Just $ NormalLineStyle $ NormalLineStyleData 1 0.2 0.5,
         Link "node1" "node3" Nothing, Link "node2" "node3" Nothing,
         Link "node2" "node4" Nothing, Link "node1" "node4" Nothing]
