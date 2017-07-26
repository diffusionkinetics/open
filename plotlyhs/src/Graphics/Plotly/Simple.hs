{-|
Functions to build Traces from standard data. Generated traces can still be
customized with lenses.
-}
module Graphics.Plotly.Simple where

import Data.Aeson
import Data.Text (Text)
import Lens.Micro

import Graphics.Plotly.Base


-- |Generate a scatterplot from pairs
scatterPlot :: (ToJSON a, ToJSON b) => [(a, b)] -> Trace
scatterPlot xys = scatter
    & x     ?~ fmap (toJSON . fst) xys
    & y     ?~ fmap (toJSON . snd) xys
    & mode  ?~ [Markers]


-- |Generate a line plot from pairs
linePlot :: (ToJSON a, ToJSON b) => [(a, b)] -> Trace
linePlot xys = scatter
    & x     ?~ fmap (toJSON . fst) xys
    & y     ?~ fmap (toJSON . snd) xys
    & mode  ?~ [Lines]


-- |Generate a horizontal bar chart from pairs of text and value.
hbarChart :: [(Text, Double)] -> Trace
hbarChart tvs = bars
    & y             ?~ fmap (toJSON . fst) tvs
    & x             ?~ fmap (toJSON . snd) tvs
    & orientation   ?~ Horizontal


-- |Generate a horizontal bar chart from pairs of text and value.
vbarChart :: [(Text, Double)] -> Trace
vbarChart tvs = bars
    & x             ?~ fmap (toJSON . fst) tvs
    & y             ?~ fmap (toJSON . snd) tvs
    & orientation   ?~ Vertical


-- |Generate a fan plot with a given width in standard deviations and
--  (x,(y,sd)) data
fanPlot :: Double -> [(Double, (Double, Double))] -> Trace
fanPlot sdCount tmnsds = scatter
    & x     ?~ fmap toJSON xs
    & y     ?~ fmap toJSON ys
    & fill  ?~ ToZeroY
  where
    xs = fmap fst tmnsds ++ reverse (fmap fst tmnsds)
    ys = fmap ((\(m, sd) -> m + sdCount * sd) . snd) tmnsds
            ++ reverse (fmap ((\(m, sd) -> m - sdCount * sd) . snd) tmnsds)

