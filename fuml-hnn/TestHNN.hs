module Main where

import AI.HNN.FF.Network
import Control.Arrow
import Numeric.LinearAlgebra
import Numeric.Datasets.OldFaithful
import Numeric.Datasets.Iris
import qualified Data.Vector.Storable as V
import Data.Function (on)
import Data.Random
import Fuml.Core
import Fuml.Supervised.Accuracy
import Fuml.HNN

threshold x | x >= 0.5 = 1 | otherwise = 0

irisSample :: Iris -> Sample R
irisSample (Iris a b c d cl) = (vector [a,b,c,d], vector (list cl)) where
  list Setosa = [1,0,0]
  list Versicolor = [0,1,0]
  list Virginica = [0,0,1]

irisOneClass :: Iris -> Sample R
irisOneClass (Iris a b c d cl) = (vector [a,b,c,d], V.singleton . toNum $ cl) where
  toNum Setosa = 1
  toNum Versicolor = 0
  toNum Virginica = 0

irisOneClass' :: Iris -> (Vector R, Bool)
irisOneClass' (Iris a b c d cl) = (vector [a,b,c,d], toNum cl) where
  toNum Setosa = True
  toNum Versicolor = False
  toNum Virginica = False

irisMultiClass :: Iris -> Sample R
irisMultiClass (Iris a b c d cl) = (vector [a,b,c,d], V.singleton . toNum $ cl) where
  toNum Setosa = 1
  toNum Versicolor = -1
  toNum Virginica = 0

ofsample :: OldFaithful -> Sample R
ofsample (OF a b c) = (vector [fromIntegral a, b], V.singleton c)

ofreg (OF a b c) = (vector [fromIntegral a, b], c)

shuffleSplitTo f = fmap (splitAt 50) . sample . shuffle . fmap f

nnRegressOldFaithful = do
  (test, train) <- shuffleSplitTo ofsample oldFaithful 

  nnet <- flip (trainNTimes 2000 0.8 sigmoid sigmoid') train <$> createNetwork 2 [2,2] 1

  let go (i,o) = do
        print "-------"
        print o
        print (output nnet sigmoid i)
        print $ "o / i:" ++ show (o / output nnet sigmoid i)
  
  mapM_ go test

nnClassifyOneClass = do
  (test, train) <- shuffleSplitTo irisOneClass iris
  
  nnet <- flip (trainUntilErrorBelow 0.2 2 sigmoid sigmoid') train <$> createNetwork 4 [2,5] 1

  let go (i, o) = do
        print "----"
        print o
        print $ output nnet sigmoid i

  mapM_ go test

testRegression = do
  (test, train) <- shuffleSplitTo ofreg oldFaithful

  let sc = Just $ Scale (vector [1/30, 1/100]) (1/7)

  reg <- runSupervisor (oneOutput [4] (tanhNTimes 1000 0.2) sc) Nothing train
  reg' <- runSupervisor (oneOutput [] (sigmoidNTimes 1000 0.2) sc) (Just $ model reg) train

  print "regression rmse"
  print $ rmse test reg'

testMultiClass = do
  (test, train) <- shuffleSplitTo irisMultiClass iris

  let sc = Just $ vector [1/8, 1/5, 1/7, 1/2.5]

  cl  <- runSupervisor (multiClass [10] (sigmoidNTimes 4000 0.99) sc) Nothing train
  cl' <- runSupervisor (multiClass [10] (sigmoidNTimes 4000 0.99) sc) (Just $ model cl) train
  let go (i,o) = do
        print "---" 
        print o
        print $ predict cl i

  print "multiclass acc"
  print $ accuracy test cl'

  {-mapM_ go test-}

testMultipleOutputsOnClassification = do
  (test, train) <- shuffleSplitTo irisSample iris

  let sc = Just $ Scale (vector [1/8, 1/5, 1/7, 1/2.5]) 1
  
  nOuts <- runSupervisor (multipleOutputs [12] (sigmoidNTimes 4000 0.99) sc) Nothing train
  nOuts' <- runSupervisor (multipleOutputs [12] (sigmoidNTimes 4000 0.99) sc) (Just $ model nOuts) train
  let go (i,o) = do
        print "---" 
        print o
        print $ predict nOuts i
  {-mapM_ go test-}

  print "multiclass acc"
  print $ accuracy test (cmap threshold <$> nOuts)

testOneClass = do
  (test, train) <- shuffleSplitTo irisOneClass' iris
  let sc = Just $ vector [1/8, 1/5, 1/7, 1/2.5]

  oneC <- runSupervisor (oneClass [4] (tanhNTimes 1000 0.9) sc) Nothing train
  oneC' <- runSupervisor (oneClass [4] (tanhNTimes 1000 0.9) sc) (Just $ model oneC) train

  print " oneClass acc "
  print $ accuracy test oneC'

{-testOverlappingClasses = do-}
  

main = do
  {-nnRegressOldFaithful-}
  {-nnClassifyOneClass-}
  {-nnClassify-}
  {-testRegression-}
  testOneClass
  {-testMultiClass-}
  {-testMultipleOutputsOnClassification -}
  print "done"
