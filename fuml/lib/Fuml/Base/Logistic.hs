module Fuml.Base.Logistic where

import Numeric.LinearAlgebra
import qualified Data.Vector.Storable as VS
import Fuml.Optimisation.BFGS
import Data.List (foldl1')

-- maybe follow this: https://idontgetoutmuch.wordpress.com/2013/04/30/logistic-regression-and-automated-differentiation-3/

logit :: Floating a => a -> a
logit x = 1 / (1 + exp (negate x))

logLikelihood1 ::  Vector Double -> Vector Double -> Bool -> Double
logLikelihood1 theta x y = ind y * log (logit z) +
                           (1 - ind y) * log (1 - logit z)
  where
    z = VS.sum $ VS.zipWith (*) theta x

-- negative log likelihood
logLikelihood :: Double -> [(Vector Double, Bool)] -> Vector Double -> Double
logLikelihood delta theData theta = negate $ (a - delta*b)/l where
  l = fromIntegral $ length theData
  a = sum $ map (uncurry $ logLikelihood1 theta) theData
  b = (/2) $ VS.sum $ VS.map (^2) theta


--https://www.cs.ox.ac.uk/people/nando.defreitas/machinelearning/lecture6.pdf
gradLogLikelihood1 :: Vector Double -> Vector Double -> Bool -> Vector Double
gradLogLikelihood1 theta x y = VS.map (*(ind y - lz)) x
--                           (1 - ind y) * log (1 - logit z)
  where
    z = VS.sum $ VS.zipWith (*) theta x
    lz = logit z

gradLogLikelihood :: Double -> [(Vector Double, Bool)] -> Vector Double -> Vector Double
gradLogLikelihood delta theData theta =
  let vs = map (uncurry $ gradLogLikelihood1 theta) theData
      vsum = foldl1' vadd vs
      l = fromIntegral $ length theData
  in VS.map (negate . (/l)) vsum  `vadd` VS.map ((/l) . (*delta)) theta


logisticRegression :: [(Vector Double, Bool)] -> Vector Double
logisticRegression theData =
  let start = VS.map (const 0) $ fst $ head theData
      inisbox = VS.map (const (0.1:: Double)) $ fst $ head theData
      hessInit = ident $ size start
  in --fst $ minimizeV NMSimplex 1e-4 200 inisbox (logLikelihood theData) start

     --fst $ minimizeVD ConjugateFR 1e-10 500 0.01 0.01 (logLikelihood theData) (gradLogLikelihood theData) start -- inisbox (logLikelihood theData) start

     case bfgsWith myBOpts (logLikelihood 0 theData) (gradLogLikelihood 0 theData) start hessInit of
       Left s -> error s
       Right (p, h) -> p
      --fst $ minimizeV NMSimplex 1e-3 100 inisbox (logLikelihood theData) start

myBOpts = BFGSOpts 1e-7 2e-5 200

ind :: Bool -> Double
ind True = 1
ind False = 0

vadd :: Vector Double -> Vector Double -> Vector Double
vadd = VS.zipWith (+)
