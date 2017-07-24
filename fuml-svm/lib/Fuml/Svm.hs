module Fuml.Svm
  ( ClassifierType(..)
  , Kernel(..)
  , OneClassResult(..)
  , RegressorType(..)
  , rbfSimple
  , svc
  , svcWtd
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
rbfSimple = Supervisor . const $ predWith chehLin classify 

svc :: Ord o =>
     ClassifierType 
  -> Kernel 
  -> Supervisor Identity o (Double, SVMClassifier o) o
svc cltype kernel = Supervisor . const $ predWith (trainClassifier cltype kernel) classify

svcWtd :: Ord o => 
     ClassifierType 
  -> Kernel
  -> Supervisor Identity (Weighted o) (Double, SVMClassifier o) o
svcWtd cltype kernel = Supervisor $ \_ theData ->
  let f = factor theData

      separate :: (Weighted o, VS.Vector Double) -> ((o, Double), (o, VS.Vector Double))
      separate (Weighted w o, v) = ((o, w), (o, v))

      wtdCl = snd $ trainWtdClassifier cltype kernel weights trainingSet
      (weights, trainingSet) = unzip . map separate . vScale f $ theData

   in return $ Predict (f, wtdCl) (classify wtdCl . VS.map (*f))

predWith train interpret dataset = return $ Predict (f, svm) (interpret svm . VS.map (*f))
  where f = factor dataset
        scaled = vScale f dataset
        svm = snd . train $ scaled

factor :: [(VS.Vector Double, o)] -> Double
factor = recip . VS.maximum . fst . maximumBy (compare `on` VS.maximum . fst)

vScale = map . scaleSwap where
  scaleSwap :: Double -> (VS.Vector Double, o) -> (o, VS.Vector Double)
  scaleSwap factor = (\(v,o) -> (o, VS.map (* factor) v))

oneClass :: 
     Double -- 'nu' parameter
  -> Kernel
  -> Supervisor Identity () (Double, SVMOneClass) OneClassResult
oneClass nu kernel = Supervisor . const $ 
   predWith (trainOneClass nu kernel . map snd) inSet

svr :: 
     RegressorType
  -> Kernel
  -> Supervisor Identity Double (Double, SVMRegressor) Double
svr rtype kernel = Supervisor . const $ 
  predWith (trainRegressor rtype kernel) predictRegression
