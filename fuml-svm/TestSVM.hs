module Main where

import Numeric.Datasets.Iris 
import Numeric.Datasets.OldFaithful
import Fuml.Svm
import Fuml.Core
import Fuml.Supervised.Accuracy

import Data.Function (on)
import Data.Tuple
import AI.SVM.Simple
import Control.Arrow
import Data.Random
import qualified Data.Vector.Storable as V

toVector = swap . fmap V.fromList

toList :: Iris -> (Int, [Double])
toList (Iris a b c d cl) = (toNum cl, [a,b,c,d])  where
  toNum Virginica = 0
  toNum Setosa = -1
  toNum Versicolor = 1

oflist :: OldFaithful -> (V.Vector Double, Double)
oflist (OF a b c) = (V.fromList [fromIntegral a, b], c)

directSvmSimple :: IO ()
directSvmSimple = do
  (test, train) <- fmap (splitAt 20) . sample . shuffle . fmap toList $ iris

  let ((Result c gamma acc), cl) = chehLin train

      count :: Eq a => [(a,a)] -> Double
      count xs = on (/) (realToFrac . length) (filter (uncurry (==)) xs) xs

  print "test accuracy"
  print . count $ fmap (classify cl) <$> test
  print "training accuracy"
  print acc

testClassifier :: IO ()
testClassifier = do
  (test, train) <- fmap (splitAt 50) . sample . shuffle . fmap (toVector . toList) $ iris


  let oneclass = runSupervisor' (oneClass 0.01 Linear) $ fmap (const ()) <$> filter ((== 1) . snd) train
  let simple = runSupervisor' rbfSimple train
  let basic = runSupervisor' (svc (C 8) (Polynomial 3 3 3)) train

  let go a | a == 1 = In | otherwise = Out
  print "oneclass" 
  print $ accuracy (fmap go <$> test) oneclass 

  print "simple accuracy"
  print $ accuracy test simple

  print "basic accuracy"
  print $ accuracy test basic
  
testRegression :: IO ()
testRegression = do
  (test, train) <- fmap (splitAt 20) . sample . shuffle . fmap oflist $ oldFaithful
  
  let regr = runSupervisor' (svr (Epsilon 0.1 0.1) (Sigmoid 3e3 3e3)) train

  print $ rmse test regr

{-main = testRegression-}
{-main = testClassifier-}
main = do
  testRegression
  testClassifier
