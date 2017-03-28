import Fuml.Supervised
import Fuml.Unsupervised
import Fuml.Core
import Fuml.Base
import Fuml.Base.Logistic
import Fuml.Optimisation
import Fuml.Optimisation.SGD
import Numeric.Datasets.Iris
import qualified Data.Vector.Storable as VS
import Control.Monad
import Control.Monad.Identity
import Data.Random

irisData = map f iris where
  f i = ( irisVec i
        , irisClass i)

irisDataBin = map f iris where
  f i = ( irisVec i
        , irisClass i == Setosa)

irisVec i = VS.fromList [sepalLength i, sepalWidth i, petalLength i, petalWidth i]

main = do
  let l = runIdentity $ runSupervisor (oneVsRest (logisticBfgs 0.001)) Nothing irisData
  print $ model l
  let i1 = head iris
      i2 = last iris
  print (i1, softMax $ predict l (irisVec i1))
  print (i2, softMax $ predict l (irisVec i2))

  let clusters = cluster iris irisVec (kmeans 3)
  --forM_ clusters $ \is -> print $ map irisClass is
  --testGradients
  testSGD
  putStrLn "hello world"

sgdOpts = SGDOpts 0.01 5 200

testSGD = do
  let Predict (betaBfgs, _) _ = runIdentity $ runSupervisor (logisticBfgs 0.01) Nothing irisDataBin
  print betaBfgs
  Predict (SGDModel betaSgd losses) _
       <- sample $ runSupervisor (sgdSupervisor sgdOpts $ logisticCost 0.01) Nothing irisDataBin
  print losses
  print betaSgd

testGradients :: IO ()
testGradients = do
  let Predict (beta, _) _ = runIdentity $ runSupervisor (logisticBfgs 0.01) Nothing irisDataBin
      --beta = VS.fromList [2.6636743970209067,7.813238177371347,-11.428852736947137,-5.1733429446846255]
      (ptv, outc) =  head irisDataBin
      (ptv1, outc1) =  last irisDataBin
      (ptvnan, outcnan) =  irisDataBin!!14
      beta0 = VS.map (const (0::Double)) beta
      beta12 = VS.map (/(2::Double)) beta
      delta = 0.5
      f = logLikelihood delta irisDataBin
      f1 v = logLikelihood1 v ptv outc
      symgradf1 v = gradLogLikelihood1 v ptv outc
      symgradf v = gradLogLikelihood delta irisDataBin v
      --nanv = VS.fromList [3.146947641220465,9.257333905281397,-13.535856317260674,-6.124467708046397]
--  print2 "ll betanan" $ logLikelihood1 nanv ptvnan outcnan
--  print2 "ll betanan" $ logit $ VS.sum $ VS.zipWith (*) nanv ptvnan
--  print2 "nan data pt" $ irisDataBin!!14
  print2 "ll beta12" $ f beta12
  print2 "beta" $ beta
--  print2 "ll betahat" $ f beta
  print2 "fdgrad beta12" $ fdgrad f beta12
  print2 "symgradf beta12" $ symgradf beta12
  --print2 "f1 betahat" $ f1 beta
  --print2 "f1 beta0" $ f1 beta0
  --print2 "fdgradll betahat" $ fdgrad f beta
--  print2 "fdgradf1 beta0" $ fdgrad f1 beta0
--  print2 "symgradf1 beta0" $ symgradf1 beta0
  return ()

print2 s x = putStrLn $ s ++ ": "++show x
