{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TupleSections #-}

module Fuml.Supervised.Accuracy where

import Fuml.Core
import Numeric.LinearAlgebra
import Data.List (partition, transpose)
import Control.Parallel.Foldl hiding (sum)

accuracy :: (Eq o) => [(Vector Double, o)] -> Predict p o -> Double
accuracy theData model =
  let isCorrect (v,y) = predict model v == y
      corrects = map isCorrect theData
  in realToFrac (length $ filter id corrects) / realToFrac (length corrects)

rmse :: [(Vector Double, Double)] -> Predict p Double -> Double
rmse theData model =
  let getDev (v,y) = let d = y - predict model v in d*d
      devs = map getDev theData
  in sqrt $ sum devs / realToFrac (length devs)

varExplained :: [(Vector Double, Double)] -> Predict p Double -> Double
varExplained d p =
  let allVar = fold (premap snd variance) d
      predOutcomes = map (predict p . fst) d
      predVar = fold variance predOutcomes
  in predVar/allVar

splitData :: Int -> Int -> [a] -> ([a], [a])
splitData nfolds fold xs
  = let xfs = zip xs (cycle $ [0..(nfolds-1)])
        (zins, zouts) = partition ((==fold) . snd) xfs
    in (map fst zins,map fst zouts)

crossValidate :: Monad m
               => Int
               -> [(Vector Double, o)]
               -> ( [(Vector Double, o)] -> Predict p o1 -> Double )
               -> Supervisor m o p o1
               -> Maybe Int
               -> m Double
crossValidate nfolds theData metric super (Just fold) =
  let (testD, trainD) = splitData nfolds fold theData
      fit = runSupervisor super Nothing trainD
  in fmap (metric testD) fit
crossValidate nfolds theData metric super Nothing =
  let folds = [0..(nfolds-1)]
      accs = mapM (crossValidate nfolds theData metric super . Just) folds
  in fmap avg accs

gridSearch  ::  Monad m
               => Int
               -> [(Vector Double, o)]
               -> ( [(Vector Double, o)] -> Predict p o1 -> Double )
               -> (hyper -> Supervisor m o p o1)
               -> [hyper]
               -> Maybe Int
               -> m [(hyper,Double)]
gridSearch nfolds theData metric hsuper hypers (Just fold) =
  let (testD, trainD) = splitData nfolds fold theData
      getAcc h = fmap ((h,) . metric testD) $ runSupervisor (hsuper h) Nothing $ trainD
  in mapM getAcc hypers
gridSearch nfolds theData metric hsuper hypers Nothing = do
  let folds = [0..(nfolds-1)]
  accs <- mapM (gridSearch nfolds theData metric hsuper hypers . Just) folds
  let f accs = (fst $ head accs, avg $ map snd accs)
  return $ map f $ transpose accs

avg xs = sum xs / realToFrac (length xs)
