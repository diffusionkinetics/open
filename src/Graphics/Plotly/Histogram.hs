{-|

Simple histograms

-}

module Graphics.Plotly.Histogram where

import Graphics.Plotly
import Data.List (sort, group)
import Lens.Micro
import Data.Aeson (toJSON)

-- | build a histogram with a given binsize
histogram :: Int -- ^ number of bins
          -> [Double] -- ^ the individual observations
          -> Trace
histogram nbins pts =
  let (lo, hi) = (minimum pts, maximum pts)
      binSize = (hi - lo) / realToFrac nbins
      binf :: Double -> Int
      binf xv = floor $ (xv - lo) / binSize
      binToX :: Int -> Double
      binToX binN = realToFrac binN * binSize + lo
      bins = group $ sort $ map binf pts
      goFill (car@(bin1,_):cdr@((bin2,_):_))
         | bin2 == bin1 + 1 =  car : goFill cdr
         | otherwise = car : goFill ((bin1+1,0):cdr)
      goFill l = l
      binMap :: [(Int, Int)]
      binMap = goFill $ map (\is -> (head is, length is)) bins

  in bars & x ?~ map (toJSON . binToX . fst) binMap & y ?~ map (toJSON .  snd) binMap
