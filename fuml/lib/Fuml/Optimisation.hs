module Fuml.Optimisation where

import Fuml.Core
import Fuml.Optimisation.BFGS
import Fuml.Optimisation.SGD
import Numeric.LinearAlgebra
import qualified Data.Vector.Storable as VS
import Fuml.Base.Logistic
import Data.Maybe (fromMaybe)
import Control.Monad.Identity
import Data.Random

type BfgsModel = (Vector Double, Matrix Double)

data CostGradientModel a b = CostGradientModel
  { costFunction :: ([(Vector Double, a)] -> Vector Double -> Double)
  , costGradient :: ([(Vector Double, a)] -> Vector Double -> Vector Double)
  , generatePrediction :: Vector Double -> Vector Double -> b
  }

bfgsSupervisor :: BFGSOpts
               -> CostGradientModel a b
               -> Supervisor Identity a BfgsModel b
bfgsSupervisor bopts (CostGradientModel ll gradll predf) = Supervisor $ \mp theData ->
  let start = fromMaybe (VS.map (const 0) $ fst $ head theData) $ fmap fst mp
      hessInit = fromMaybe (ident $ size start) $ fmap snd mp
  in case bfgsWith bopts (ll theData) (gradll theData) start hessInit of
       Left s -> error s
       Right (p, h) -> return $ Predict (p, h) (predf p)

defBfgs = BFGSOpts 1e-7 1e-7 200

data SGDModel = SGDModel
  { sgdFit :: Vector Double
  , lossCurve :: [Double]
  }

sgdSupervisor :: SGDOpts
              -> CostGradientModel a b
              -> Supervisor RVar a SGDModel b
sgdSupervisor opts (CostGradientModel ll gradll predf) = Supervisor $ \mp theData ->
  let start = fromMaybe (VS.map (const 0) $ fst $ head theData) $ fmap sgdFit mp
      mkPredict sgdModel = Predict sgdModel (predf (sgdFit sgdModel))
      go 0 p losses = return $ SGDModel p $ reverse losses
      go n p losses = do
        nextp <- sgdEpoch opts gradll theData p
        let loss = ll theData p
        go (n-1) nextp (loss:losses)
  in fmap mkPredict $ go (epochs opts) start []

logisticCost :: Double -> CostGradientModel Bool Double
logisticCost delta = CostGradientModel (logLikelihood delta) (gradLogLikelihood delta) (\beta v -> logit $ v `dot` beta)

logisticBfgs :: Double -> Supervisor Identity Bool BfgsModel Double
logisticBfgs delta = bfgsSupervisor defBfgs (logisticCost delta)
