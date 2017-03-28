{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Fuml.Supervised.KNN where

import Numeric.LinearAlgebra
import Data.List (sortBy)
import Data.Ord (comparing)
import Fuml.Core
import Fuml.Base.KNN
import Control.Monad.Identity


data KNN a = KNN {
   knnPoints :: [(Vector Double, a)]
  }

generalKNN :: Int -> (Vector Double -> Vector Double -> Double) -> ([(Double, a)] -> b) -> Supervisor Identity a (KNN a) b
generalKNN k dist vote  = Supervisor $ \_ theData ->
  return $ Predict (KNN theData) $ \v ->
   let ptDist (v1, y) = (dist v v1,y)
       nns = take k $ sortBy (comparing fst) $ map ptDist $ theData
   in vote nns

binaryKNN :: Int -> Supervisor Identity Bool (KNN Bool) Double
binaryKNN k = generalKNN k euclideanDistance weightedBoolVote

catKNN :: Eq a => Int -> Supervisor Identity a (KNN a) a
catKNN k = generalKNN k euclideanDistance weightedMajorityVote

regressKNN :: Int -> Supervisor Identity Double (KNN Double) Double
regressKNN k = generalKNN k euclideanDistance weightedAvgVote
