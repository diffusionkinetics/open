module Fuml.Base.PCA where

import Numeric.LinearAlgebra
import Data.List (foldl')

sumWith :: Num a => (Vector Double -> a) -> [Vector Double] -> a
sumWith f (v:vs) = foldl' (\acc v -> acc + f v) (f v) vs

meanVs :: [Vector Double] -> Vector Double
meanVs vecs = scale (recip $ realToFrac $ length vecs) $ sumWith id vecs

covVs :: Vector Double -> [Vector Double] -> Herm Double
covVs xmn xs
   = let k = length xs
         f xi = outerSame $ xi - xmn
     in sym $ scale (recip $ realToFrac k - 1 ) $ sumWith f xs

outerSame v = outer v v

--following https://github.com/albertoruiz/hmatrix/blob/master/examples/pca2.hs
type Stat = (Vector Double, [Double], Matrix Double)

statVs :: [Vector Double] -> Stat
statVs vs = (m, toList s, tr' v) where
    m = meanVs vs
    (s,v) = eigSH (covVs m vs)

pcaN :: Int -> Stat -> (Vector Double -> Vector Double , Vector Double -> Vector Double )
pcaN n (m,s,v) = (encode,decode)
  where
    encode x = vp #> (x - m)
    decode x = x <# vp + m
    vp = takeRows n v
