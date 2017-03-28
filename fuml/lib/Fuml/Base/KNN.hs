{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Fuml.Base.KNN where

import qualified Data.Vector.Storable as VS
import Numeric.LinearAlgebra
import Data.List (nub, sortBy)
import Data.Ord (comparing)

euclideanDistance :: Vector Double -> Vector Double -> Double
euclideanDistance v1 v2 = sqrt $ VS.sum $ VS.map (^2) $ VS.zipWith (-) v1 v2

weightedBoolVote :: [(Double, Bool)] -> Double
weightedBoolVote distBools =
  let wtrue = sum $ map (recip . fst) $ filter snd distBools
      wfalse = sum $ map (recip . fst) $ filter (not . snd) distBools
  in exp wtrue / (exp wtrue + exp wfalse)

majorityVote :: Eq a => [(Double, a)] -> a
majorityVote distXs = let classes = nub $ map snd distXs
                          occurences c = (c,negate $ length $ filter ((==c) . snd) distXs)
                      in fst $ head $ sortBy (comparing snd) $ map occurences classes

weightedMajorityVote :: Eq a => [(Double, a)] -> a
weightedMajorityVote distXs
  = let classes = nub $ map snd distXs
        weight c = (c,negate $ sum $ map (recip . fst) $ filter ((==c) . snd) distXs)
        in fst $ head $ sortBy (comparing snd) $ map weight classes

avgVote :: [(Double, Double)] -> Double
avgVote distXs = let n = realToFrac $ length distXs
                 in (sum $ map snd distXs) / n

weightedAvgVote :: [(Double, Double)] -> Double
weightedAvgVote distXs
  = let wsum = sum $ map (recip . fst) distXs
    in  (sum $ map (uncurry (*)) distXs) / wsum
