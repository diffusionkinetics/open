{-|

Functions to build Traces from standard data. Generated traces can still be customized with lenses

-}

module Graphics.Plotly.Simple where

import Graphics.Plotly
import Lens.Micro
import Data.Text (Text)

-- |Generate a scatterplot from pairs
scatterPlot :: [(Double,Double)] -> Trace
scatterPlot xys = scatter & x ?~ map fst xys
                          & y ?~ map snd xys
                          & mode ?~ [Markers]
-- |Generate a line plot from pairs
linePlot :: [(Double,Double)] -> Trace
linePlot xys    = scatter & x ?~ map fst xys
                          & y ?~ map snd xys
                          & mode ?~ [Lines]

-- |Generate a horizontal bar chart from pairs of text and value.
hbarChart :: [(Text, Double)] -> Trace
hbarChart tvs = bars & ytext ?~ map fst tvs
                     & x ?~ map snd tvs
                     & orientation ?~ Horizontal
