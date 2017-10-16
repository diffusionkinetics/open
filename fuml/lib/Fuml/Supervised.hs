{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Fuml.Supervised where

import qualified Fuml.Base.LinearRegression as LR
import Fuml.Core
import Numeric.LinearAlgebra
import Control.Monad.Identity

newtype Beta = Beta { unBeta :: Vector Double }

ols :: Supervisor Identity Double Beta Double
ols = Supervisor $ \_ theData ->
              let beta = LR.ols theData
              in return $ linPredict beta

type Weight = Double

olsWeighted :: Supervisor Identity (Double, Weight) Beta Double
olsWeighted
   = Supervisor $ \_ theDataW ->
              let wvs = fromList $ map (\(_,(_,w)) -> w) theDataW
                  theData = map (\(xs,(y,_)) -> (xs,y)) theDataW
                  beta = LR.wols wvs theData
              in return $ linPredict beta

ridge :: Double -> Supervisor Identity Double Beta Double
ridge gammac = Supervisor $ \_ xys ->
  let npars = size $ fst $ head xys
      gamma = diag $ konst gammac npars
      beta = LR.ridge gamma xys
  in return $ linPredict beta

linPredict :: Vector Double -> Predict Beta Double
linPredict beta = Predict (Beta beta) (`dot` beta)
