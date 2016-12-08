{-# LANGUAGE OverloadedStrings,ExtendedDefaultRules, QuasiQuotes #-}

module Main where

import Lucid
import Lucid.Html5
import Graphics.Plotly hiding (text)
import Graphics.Plotly.Lucid
import Lucid.Bootstrap
import Data.Monoid ((<>))
import NeatInterpolation
import Lens.Micro
import Numeric.Datasets.Iris

import Data.Text (Text)

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
               row_ $ h4_ "A complete & minimal example"
               row_ $ do
                 div_ [class_ "col-md-6"] $ pre_ $ code_ $ toHtml
                  [text|
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

                        myTrace = scatter & x ?~ [1,2,3,4]
                                          & y ?~ [500,3000,700,200] |]
                 div_ [class_ "col-md-6"] $ toHtml $ plotly "p0" [myTrace]
               row_ $ p_ $ "In the examples below, we omit all of the imports, main function, html header and focus only"<>
                           " on the "<> code_ "Plotly" <> " value (the argument to "<> code_ "toHtml"<>"). The "<>
                           code_ "Plotly" <> " value can be constructed with the function "<> code_ "plotly" <>
                           " which takes two arguments: the element id of the "<> code_ "<div>" <>
                           " for the plot (this element will be created if you call toHtml on the "<> code_ "Plotly" <>
                           " value) and a list of traces."

               row_ $ h4_ "A simple plot"
               row_ $ do
                 div_ [class_ "col-md-6"] $ pre_ $ code_ $ toHtml
                  [text|
                    let myTrace
                          = scatter & x ?~ [1,2,3,4]
                                    & y ?~ [500,3000,700,200]
                   in plotly "div1" [myTrace] |]
                 div_ [class_ "col-md-6"] $ toHtml $ plotly "p1" [myTrace]
               row_ $ p_ $ "Note that Plotlyjs considers a line plot to be a kind of scatter plot, which may not "<>
                    "be the terminology you are used to. The above is quite unbearably sized & padded for this tutorial, so let's fix the " <>
                    "margins and the plot height"
               row_ $ do
                 div_ [class_ "col-md-6"] $ pre_ $ code_ $ toHtml
                  [text|
                    let myTrace
                          = scatter & x ?~ [1,2,3,4]
                                    & y ?~ [500,3000,700,200]
                   in plotly "div2" [myTrace] & layout . margin ?~ thinMargins
                                              & layout . height ?~ 300  |]
                 div_ [class_ "col-md-6"] $ toHtml $ plotly "p2" [myTrace] & layout . margin ?~ thinMargins
                                                                           & layout . height ?~ 300
               row_ $ h4_ "Lines and Markers"
               row_ $ do
                 div_ [class_ "col-md-6"] $ pre_ $ code_ $ toHtml
                  [text|
                    let myTrace
                          = scatter & x ?~ [1,2,3,4]
                                    & y ?~ [500,3000,700,200]

                   in plotly "div3"
                             [myTrace & mode ?~ [Markers]]
                             & layout . margin ?~ thinMargins
                             & layout . height ?~ 300  |]
                 div_ [class_ "col-md-6"] $ toHtml $ plotly "div3" [myTrace & mode ?~ [Markers]]
                                                                           & layout . margin ?~ thinMargins
                                                                           & layout . height ?~ 300
               row_ $ do
                 div_ [class_ "col-md-6"] $ pre_ $ code_ $ toHtml
                  [text|
                    let myTrace
                          = scatter & x ?~ [1,2,3,4]
                                    & y ?~ [500,3000,700,200]

                   in plotly "div4"
                             [myTrace & mode ?~ [Lines]]
                             & layout . margin ?~ thinMargins
                             & layout . height ?~ 300  |]
                 div_ [class_ "col-md-6"] $ toHtml $ plotly "div4" [myTrace & mode ?~ [Lines]]
                                                                           & layout . margin ?~ thinMargins
                                                                           & layout . height ?~ 300
               row_ $ do
                 div_ [class_ "col-md-6"] $ pre_ $ code_ $ toHtml
                  [text|
                    let myTrace
                          = scatter & x ?~ [1,2,3,4]
                                    & y ?~ [500,3000,700,200]

                   in plotly "div5"
                             [myTrace & mode ?~ [Lines,Markers]]
                             & layout . margin ?~ thinMargins
                             & layout . height ?~ 300  |]
                 div_ [class_ "col-md-6"] $ toHtml $ plotly "div5" [myTrace & mode ?~ [Lines,Markers]]
                                                                           & layout . margin ?~ thinMargins
                                                                           & layout . height ?~ 300
               row_ $ h4_ "Iris plots"
               row_ $ p_ "This plot uses the iris value from the datasets package"
               row_ $ do
                 div_ [class_ "col-md-6"] $ pre_ $ code_ $ toHtml
                  [text|
                    plotly "div6"
                           [scatter & x ?~ map sepalLength iris
                                    & y ?~ map sepalWidth iris
                                    & marker ?~
                                       (defMarker
                                           & markercolor ?~ catColors (map irisClass
                                                                           iris))
                                    & mode ?~ [Markers]]
                            & layout . margin ?~ thinMargins
                            & layout . height ?~ 300 |]
                 div_ [class_ "col-md-6"] $ toHtml $ plotly "div6"
                           [scatter & x ?~ map sepalLength iris
                                    & y ?~ map sepalWidth iris
                                    & marker ?~ (defMarker & markercolor ?~ catColors (map irisClass iris))
                                    & mode ?~ [Markers]]
                            & layout . margin ?~ thinMargins
                            & layout . height ?~ 300
               row_ $ h4_ "Horizontal bar plots"
               row_ $ do
                 div_ [class_ "col-md-6"] $ pre_ $ code_ $ toHtml
                  [text|
                    let hbarData :: [(Text, Double)]
                        hbarData =  [("Simon", 14.5), ("Joe", 18.9), ("Dorothy", 16.2)]

                    in plotly "div7"
                           [bars & ytext ?~ map fst hbarData
                                 & x ?~ map snd hbarData
                                 & orientation ?~ Horizontal]
                           & layout . margin ?~ thinMargins
                           & layout . height ?~ 300|]
                 div_ [class_ "col-md-6"] $ toHtml $ plotly "div7"
                       [bars & ytext ?~ map fst hbarData
                             & x ?~ map snd hbarData
                             & orientation ?~ Horizontal]
                       & layout . margin ?~ thinMargins
                            & layout . height ?~ 300

             script_ [src_ "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"] ""
             script_ [src_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"] ""


hbarData :: [(Text, Double)]
hbarData = [("Simon", 14.5), ("Joe", 18.9), ("Dorothy", 16.2)]



myTrace
  = scatter & x ?~ [1,2,3,4]
            & y ?~ [500,3000,700,200]
