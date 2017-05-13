{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Fuml.Supervised where

import qualified Fuml.Base.LinearRegression as LR
import qualified Fuml.Base.Logistic as LG
import Fuml.Core
import Numeric.LinearAlgebra
import Control.Monad.Identity


ols :: Supervisor Identity Double (Vector Double) Double
ols = Supervisor $ \_ theData ->
              let beta = LR.ols theData
              in return $ linPredict beta

olsWeighted :: Vector Double -> Supervisor Identity Double (Vector Double) Double
olsWeighted wvs
   = Supervisor $ \_ theData ->
              let beta = LR.wols wvs theData
              in return $ linPredict beta

ridge :: Double -> Supervisor Identity Double (Vector Double) Double
ridge gammac = Supervisor $ \_ xys ->
  let npars = size $ fst $ head xys
      gamma = diag $ konst gammac npars
      beta = LR.ridge gamma xys
  in return $ linPredict beta

linPredict :: Vector Double -> Predict (Vector Double) Double
linPredict beta = Predict beta (`dot` beta)
