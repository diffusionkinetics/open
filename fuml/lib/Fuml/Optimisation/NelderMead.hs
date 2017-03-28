{-# LANGUAGE ScopedTypeVariables, RecordWildCards, FlexibleContexts #-}


module Fuml.Optimisation.NelderMead where

import qualified Data.Vector.Storable as VS
import Data.Vector.Storable (Vector)
import Data.List (foldl1', sortBy)
import Data.Ord (comparing)
import Data.Random
import Control.Monad (replicateM, forM)
import Control.Monad.Trans (lift)

type Simplex = [(Vector Double, Double)]

centroid :: Simplex -> Vector Double
centroid  points = VS.map (/n) $ sumVs $ map fst points
    where n = realToFrac $ length points

sumVs :: [Vector Double] -> Vector Double
sumVs = foldl1' vadd

vadd, vsub :: Vector Double -> Vector Double -> Vector Double
vadd = VS.zipWith (+)
vsub = VS.zipWith (-)

sortSimplex :: Simplex -> Simplex
sortSimplex = sortBy (comparing snd)

initialSimplex :: Monad m => (Vector Double -> m Double)
                          -> Vector Double
                          -> Vector Double
                          -> RVarT m Simplex
initialSimplex f vinit vbox = do
  let n = VS.length vinit + 1
  fmap sortSimplex $ replicateM n $ do
    let gen mid box = uniformT (mid - box) (mid+box)
    v <- VS.zipWithM gen vinit vbox
    y <- lift $ f v
    return (v, y)


solveNm :: Monad m => (Vector Double -> m Double)
                   -> Simplex
                   -> Double
                   -> Int
                   -> m Simplex
solveNm f s0 tol maxiter = go s0 0 where
  go sim iter | iter > maxiter = return sim
              | otherwise = do
                  snext <- nmStep f sim
                  go (sortSimplex snext) (iter+1)


nmStep :: Monad m => (Vector Double -> m Double) -> Simplex -> m Simplex
nmStep f  s0 = do
  let alpha = 1
      gamma = 2
      rho = 0.5
      sigma = 0.5
      x0 = centroid $ init s0
      xnp1 = fst (last s0)
      fxnp1 = snd (last s0)
      xr = x0 `vadd` (alpha .* (x0 `vsub` xnp1))
      fx1 = snd $ head s0
  fxr <- f xr

  if fx1 <= fxr && fxr <= (snd $ penultimate s0)
    then return $ swapLast s0 (xr,fxr)
    else do
        let xe = x0 `vadd` (gamma .* (x0 `vsub` xnp1))
        fxe <- f xe
        if fxr < fx1
            then if fxe < fxr
                    then return $ swapLast s0 (xe,fxe)
                    else return $ swapLast s0 (xr,fxr)
            else do
              let xc = xnp1 `vadd` (rho .* (x0 `vsub` xnp1))
              fxc <- f xc
              if fxc < fxnp1
                   then return $ swapLast s0 (xc,fxc)
                   else --reduce
                     case s0 of
                       p0@(x1,_):rest -> do
                            morePts <- forM rest $ \(xi,_) -> do
                                let xnew = x1 `vadd` (rho .* (xi`vsub`x1))
                                y <- f xnew
                                return (xnew,y)
                            return $ p0 : morePts
    where
      penultimate :: [a] -> a
      penultimate (x:_:[]) = x
      penultimate (_:xs) = penultimate xs

      swapLast :: [a] -> a -> [a]
      swapLast xs x = init xs ++ [x]


infixr 7 .*
(.*) :: Double -> Vector Double -> Vector Double
x .* v = VS.map (*x) v
