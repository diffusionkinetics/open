module Fuml.Svm
  ( ClassifierType(..)
  , Kernel(..)
  , OneClassResult(..)
  , RegressorType(..)
  , rbfSimple
  , svc
  , svr
  , oneClass
  ) where

import AI.SVM.Simple
import Data.List (maximumBy)
import Data.Function (on)
import Control.Monad.Identity (Identity)
import Control.DeepSeq (NFData)
import Fuml.Core
import qualified Data.Vector.Storable as VS

rbfSimple :: (Ord o, NFData o) => Supervisor Identity o (Double, SVMClassifier o) o
rbfSimple = Supervisor $ \_ theData ->
  let scaled = vScale f theData
      f = factor theData
   in return . predWith f . snd . chehLin $ scaled

svc :: Ord o =>
     ClassifierType 
  -> Kernel 
  -> Maybe [(o, Double)] -- Training weights
  -> Supervisor Identity o (Double, SVMClassifier o) o
svc cltype kernel mbweights = Supervisor $ \_ theData ->
  let scaled = vScale f theData
      f = factor theData

      classifier = case mbweights of 
        Just weights -> snd $ trainWtdClassifier cltype kernel weights scaled
        Nothing -> snd $ trainClassifier cltype kernel scaled
   in return (predWith f classifier)

predWith f cl = Predict (f, cl) (classify cl . VS.map (*f))

factor :: [(VS.Vector Double, o)] -> Double
factor = recip . VS.maximum . fst . maximumBy (compare `on` VS.maximum . fst)

vScale = map . scaleSwap where
  scaleSwap :: Double -> (VS.Vector Double, o) -> (o, VS.Vector Double)
  scaleSwap factor = (\(v,o) -> (o, VS.map (* factor) v))

oneClass :: 
     Double -- 'nu' parameter
  -> Kernel
  -> Supervisor Identity () (Double, SVMOneClass) OneClassResult
oneClass nu kernel = Supervisor $ \_ theData ->
  let f = factor theData
      scaled = VS.map (*f) . fst <$> theData
      oneclass = snd $ trainOneClass nu kernel scaled
   in return $ Predict (f, oneclass) (inSet oneclass . VS.map (*f))

svr :: 
     RegressorType
  -> Kernel
  -> Supervisor Identity Double (Double, SVMRegressor) Double
svr rtype kernel = Supervisor $ \_ theData ->
  let f = factor theData
      scaled = vScale f theData
      regressor = snd $ trainRegressor rtype kernel scaled
   in return $ Predict (f, regressor) (predictRegression regressor . VS.map (*f))
