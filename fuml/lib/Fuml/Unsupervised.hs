{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TupleSections, FlexibleContexts #-}

module Fuml.Unsupervised where

import Fuml.Core
import qualified Fuml.Base.PCA as PCA
import Fuml.Base.KNN (euclideanDistance)
import Numeric.LinearAlgebra
import qualified Math.KMeans as KM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.List (foldl1', sortBy, nub)
import Data.Ord (comparing)
import Control.Monad.Identity


type Unsupervisor m p a = Supervisor m () p a

with :: b -> [a] -> [(a,b)]
with y = map (,y)

data PCA = PCA { pcaComponents :: Int, pcaStat :: PCA.Stat }

pca :: Int -> Unsupervisor Identity PCA.Stat (Vector Double)
pca ncomp  = Supervisor $ \_ theData ->
  let stat@(m,_,v) = PCA.statVs $ map fst theData
  in return $ Predict stat $ \x -> takeRows ncomp v #> (x - m)

findNearestCentroidIx :: [Vector Double] -> Vector Double -> Int
findNearestCentroidIx ctrs x =
  let vdixs = zip [0..] $ map (\c -> euclideanDistance c x) ctrs
  in fst $ head $ sortBy (comparing snd) vdixs

centroid :: [Vector Double] -> Vector Double
centroid vs =
  let vadd = VS.zipWith (+)
      n = realToFrac $ length vs
  in VS.map (/n) $ foldl1' vadd vs

kmeans :: Int -> Unsupervisor Identity [Vector Double] Int
kmeans nclus
  =  Supervisor $ \_ theData ->
        let clus = KM.kmeans (VU.convert) KM.euclidSq nclus (map fst theData)
            ctrs = map (centroid . KM.elements) $ V.toList clus
            pr v = findNearestCentroidIx ctrs v
        in return $ Predict ctrs pr

cluster :: Eq b => [a] -> (a-> Vector Double) -> Unsupervisor Identity p b -> [[a]]
cluster xs f unsup =
  let p = runIdentity $ runSupervisor unsup Nothing $ with () $ map (f) xs
      withClus x = (x,predict p $ f x)
      withCluss = map withClus xs
      cluss = nub $ map snd withCluss
      getElems clus = map fst $ filter ((==clus) . snd) withCluss
  in map getElems cluss
