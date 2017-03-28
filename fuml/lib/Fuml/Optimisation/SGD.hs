module Fuml.Optimisation.SGD where

import Numeric.LinearAlgebra
import Data.Random

data SGDOpts = SGDOpts
  { learning_rate :: Double
  , batch_size :: Int
  , epochs :: Int
  }

sgdBatch :: SGDOpts -> (Vector Double -> Vector Double) -> Vector Double -> Vector Double
sgdBatch opts grad p = p - scale (learning_rate opts) (grad p)

sgdEpoch :: SGDOpts -> ([a] -> Vector Double -> Vector Double) -> [a] -> Vector Double -> RVar (Vector Double)
sgdEpoch opts grad pts' p' = do
  pts <- shuffle pts'
  return $ go p' $ inGroupsOf (batch_size opts) pts
    where go p [] = p
          go p (batch:bs) = go (sgdBatch opts (grad batch) p) bs

inGroupsOf :: Int -> [a] -> [[a]]
inGroupsOf n [] = []
inGroupsOf n xs = let (these,those) = splitAt n xs
                  in these : inGroupsOf n those
