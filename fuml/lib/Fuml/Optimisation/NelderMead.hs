{-# LANGUAGE ScopedTypeVariables, RecordWildCards, FlexibleContexts #-}

module Fuml.Optimisation.NelderMead
  ( centroid
  , initialSimplex
  , initialSimplex'
  , solveNm
  , Simplex
  ) where

import qualified Data.Vector.Storable as VS
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable.Mutable as MVS (unsafeModify)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.STRef
import Data.Random
import Control.Monad (replicateM, forM, forM_)
import Control.Monad.ST
import Control.Monad.Trans (lift)

type Simplex = [(Vector Double, Double)]

centroid :: Simplex -> Vector Double
centroid [] = error "Fuml.Optimisation.NelderMead.centroid: empty Simplex input"
centroid ((p, _) : ps) = runST $ do
  vec <- VS.thaw p
  let plen = VS.length p
  forM_ ps $ \(v, _) ->
    let vlen = min plen (VS.length v)
        go n | n >= vlen = pure ()
             | otherwise = do
                 MVS.unsafeModify vec (+ v VS.! n) n
                 go (n + 1)
    in go 0
  let len = realToFrac $ length ps +1
  VS.map (/len) <$> VS.unsafeFreeze vec

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
  let gen mid box = uniformT (mid - box) (mid+box)
  initialSimplex' f $ VS.zipWithM gen vinit vbox

initialSimplex' :: Monad m => (Vector Double -> m Double)
                          -> RVarT m (Vector Double)
                          -> RVarT m Simplex
initialSimplex' f vgen = do
  v0 <- vgen
  let n = VS.length v0 + 1
  fmap sortSimplex $ replicateM n $ do
    v <- vgen
    y <- lift $ f v
    return (v, y)

solveNm :: Monad m => (Vector Double -> m Double)
                   -> Simplex
                   -> Double
                   -> Int
                   -> m ([Double], Simplex)
solveNm f s0 tol maxiter = go [] s0 0 where
  go path sim iter | iter > maxiter
                     || (iter > maxiter `div` 5
                         && abs ((snd $ head sim) - (snd $ last sim)) < tol)
                          = return (path,sim)
                   | otherwise = do
                        snext <- nmStep f sim
                        go (snd (head snext):path) (sortSimplex snext) (iter+1)


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
