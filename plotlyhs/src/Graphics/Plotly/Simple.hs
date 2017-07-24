{-# LANGUAGE OverloadedStrings #-}

{-|

Functions to build Traces from standard data. Generated traces can still be customized with lenses

-}

module Graphics.Plotly.Simple where

import Graphics.Plotly.Base
import Lens.Micro
import Data.Text (Text)
import Data.Aeson


-- |Generate a scatterplot from pairs
scatterPlot :: [(Double,Double)] -> Trace
scatterPlot xys = scatter & x ?~ map (toJSON .fst) xys
                          & y ?~ map (toJSON .snd) xys
                          & mode ?~ [Markers]
-- |Generate a line plot from pairs
linePlot :: [(Double,Double)] -> Trace
linePlot xys    = scatter & x ?~ map (toJSON .fst) xys
                          & y ?~ map (toJSON .snd) xys
                          & mode ?~ [Lines]

-- |Generate a horizontal bar chart from pairs of text and value.
hbarChart :: [(Text, Double)] -> Trace
hbarChart tvs = bars & y ?~ map (toJSON . fst) tvs
                     & x ?~ map (toJSON . snd) tvs
                     & orientation ?~ Horizontal

-- |Generate a horizontal bar chart from pairs of text and value.
vbarChart :: [(Text, Double)] -> Trace
vbarChart tvs = bars & x ?~ map (toJSON . fst) tvs
                     & y ?~ map (toJSON . snd) tvs
                     & orientation ?~ Vertical

-- |Generate a fan plot with a given width in standard deviations and
--  (x,(y,sd)) data
fanPlot :: Double -> [(Double, (Double, Double))] -> Trace
fanPlot sdCount tmnsds =
  let xs = map fst tmnsds ++ reverse (map fst tmnsds)
      ys = map ((\(m,sd) -> m+sdCount*sd) . snd) tmnsds
           ++ reverse ( map ((\(m,sd) -> m-sdCount*sd) . snd) tmnsds)
  in  scatter & x ?~ map toJSON xs & y ?~ map toJSON ys & fill ?~ ToZeroY
