{-|

Simple histograms

-}

module Graphics.Plotly.Histogram where

import Graphics.Plotly
import Data.List (sort, group)
import Lens.Micro

-- | build a histogram with a given binsize
histogram :: Int -- ^ number of bins
          -> [Double] -- ^ the individual observations
          -> Trace
histogram nbins pts =
  let (lo, hi) = (minimum pts, maximum pts)
      binSize = (hi - lo) / realToFrac nbins
      binf :: Double -> Int
      binf x = floor $ (x - lo) / binSize
      binToX binN = realToFrac binN * binSize + lo
      bins = group $ sort $ map binf pts
      goFill (car@(bin1,count1):cdr@((bin2,count2):_))
         | bin2 == bin1 + 1 =  car : goFill cdr
         | otherwise = car : goFill ((bin1+1,0):cdr)
      goFill l = l
      binMap = goFill $ map (\is -> (head is, length is)) bins

  in bars & x ?~ map (binToX . fst) binMap & y ?~ map (realToFrac . snd) binMap
