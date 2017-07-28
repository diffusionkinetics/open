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
     div_ [id_ "cy"] ""
     script_ $ runEcharts element options
