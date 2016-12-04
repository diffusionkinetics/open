{-# LANGUAGE OverloadedStrings,ExtendedDefaultRules #-}

import Lucid
import Lucid.Html5
import Graphics.Plotly
import Graphics.Plotly.Lucid
import Lens.Micro

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

main = T.writeFile "plotlyTest.html" $ renderText $ doctypehtml_ $ do
  head_ $ do meta_ [charset_ "utf-8"]
             script_ [src_ "https://cdn.plot.ly/plotly-latest.min.js"] ""
--  head_ $ script_ "hello"
  body_ $ do p_ "hello world"
             toHtml $ plotly "myDiv" [myTrace] & layout . title ?~  "my plot"
                                               & layout . margin ?~  titleMargins
             p_ "hello world"

myTrace = scatter & x ?~ [1,2,3,4]
                  & y ?~ [500,3000,700,200]
                  & mode ?~ [Markers]
