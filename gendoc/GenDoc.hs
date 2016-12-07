{-# LANGUAGE OverloadedStrings,ExtendedDefaultRules #-}

module Main where

import Lucid
import Lucid.Html5
import Graphics.Plotly
import Graphics.Plotly.Lucid
import Lucid.Bootstrap
import Data.Monoid ((<>))

import Lens.Micro

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

main = T.writeFile "../docs/index.html" $ renderText $ doctypehtml_ $ do
  head_ $ do meta_ [charset_ "utf-8"]
             link_ [rel_ "stylesheet",
                    href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"]
             link_ [rel_ "stylesheet",
                    href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"]

             plotlyCDN
  body_ $ do container_ $ do
               row_ $ do
                 h2_ "Plotly.js Haskell bindings examples"
                 p_ $ "This web pages shows plots generated with the plotlyhs packages, along with the " <>
                      "Haskell code that generated the plots. To use the plots on the generate page, "<>
                      "the Plotly.js source code should first be including by adding this tag to your HTML "<>
                      "header:"
                 pre_ $ code_ $ "<script src=\"https://cdn.plot.ly/plotly-latest.min.js\"></script>"
                 p_ $ "Alternatively, this tag can be included in an HTML page using the "<>code_ "plotlyCDN"<>
                      " function in "<>code_ "Graphics.Plotly.Lucid"<>" (when using Lucid) or "<>
                      code_ "Graphics.Plotly.Blaze" <> " (when using blaze-html)."
               row_ $ h4_ "Scatter plot"
               row_ $ do
                 div_ [class_ "col-md-6"] $ pre_ $ code_
                   "let myTrace \n\
                   \      = scatter & x ?~ [1,2,3,4] \n\
                   \                & y ?~ [500,3000,700,200] \n\
                   \                & mode ?~ [Markers] \n\n \
                   \in toHtml $ \n\
                   \      plotly \"myDiv\" [myTrace] "
                 div_ [class_ "col-md-6"] $ toHtml $ plotly "myDiv" [myTrace] 
               p_ "hello world"
             script_ [src_ "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"] ""
             script_ [src_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"] ""




myTrace
  = scatter & x ?~ [1,2,3,4]
            & y ?~ [500,3000,700,200]
            & mode ?~ [Markers]
