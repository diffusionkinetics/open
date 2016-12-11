{-| This module provides efficient, streaming and parallel left folds that you can combine
    using 'Applicative' style.

-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Control.Parallel.Foldl where

import Control.Applicative
--import Data.Foldable (Foldable)
import qualified Data.Vector.Generic as VG
import qualified Data.Map.Strict as Map
import Data.List (foldl1')
import Data.Map.Strict (Map)
import Data.Profunctor
import Control.Monad.Par
import Prelude hiding
    ( head
    , last
    , null
    , length
    , and
    , or
    , all
    , any
    , sum
    , product
    , maximum
    , minimum
    , elem
    , notElem
    , filter
    )

import qualified Data.Foldable               as F
--import qualified Data.List                   as List


{-| Efficient representation of a left fold that preserves the fold's step
    function, initial accumulator, extraction and combine function
    This allows the 'Applicative' instance to assemble derived folds that
    traverse the container only once
    A \''Fold' a b\' processes elements of type __a__ and results in a
    value of type __b__.
-}
data Fold a b
  -- | @Fold @ @ step @ @ initial @ @ extract @ @ combine@
  = forall x. Fold (x -> a -> x) x (x -> b) (x-> x-> x)

instance Functor (Fold a) where
    fmap f (Fold step begin done comb) = Fold step begin (f . done) comb
    {-# INLINE fmap #-}

instance Profunctor Fold where
    lmap = premap
    rmap = fmap

instance Applicative (Fold a) where
    pure b    = Fold (\() _ -> ()) () (\() -> b) (\() () -> ())
    {-# INLINE pure #-}

    (Fold stepL beginL doneL combL) <*> (Fold stepR beginR doneR combR) =
        let step (Pair xL xR) a = Pair (stepL xL a) (stepR xR a)
            begin = Pair beginL beginR
            done (Pair xL xR) = doneL xL (doneR xR)
            comb (Pair xL xR) (Pair yL yR) = Pair (combL xL yL) (combR xR yR)
        in  Fold step begin done comb
    {-# INLINE (<*>) #-}

data Pair a b = Pair !a !b

{-| @(premap f folder)@ returns a new 'Fold' where f is applied at each step

> fold (premap f folder) list = fold folder (map f list)

>>> fold (premap Sum mconcat) [1..10]
Sum {getSum = 55}

>>> fold mconcat (map Sum [1..10])
Sum {getSum = 55}

> premap id = id
>
> premap (f . g) = premap g . premap f

> premap k (pure r) = pure r
>
> premap k (f <*> x) = premap k f <*> premap k x
-}

premap :: (a -> b) -> Fold b r -> Fold a r
premap f (Fold step begin done comb) = Fold step' begin done comb
  where
    step' x a = step x (f a)
{-# INLINABLE premap #-}

sum :: Num a => Fold a a
sum = Fold (+) 0 id (+)
{-# INLINABLE sum #-}

sumSqr :: Num a => Fold a a
sumSqr = premap sq sum where sq x = x * x

-- | Apply a strict left 'Fold' to a 'Foldable' container
fold :: Foldable f => Fold a b -> f a -> b
fold (Fold step begin done _) as = F.foldr cons done as begin
  where
    cons a k x = k $! step x a
{-# INLINE fold #-}

foldPar :: (VG.Vector v a, Foldable v) => Int -> Fold a b -> v a -> b
foldPar nthreads (Fold step begin done comb) v = runPar $ do
  let vs = vectorSlices nthreads v
      fv v' = return $! F.foldr cons id v' begin
      cons a k x = k $! step x a
  ivs <- mapM (spawn_ . fv) vs
  xs <- mapM get ivs
  return $ done $ foldl1' comb xs


vectorSlices :: VG.Vector w a => Int -> w a -> [w a]
vectorSlices nsegs v = go v where
  seglen = ceiling $ realToFrac (VG.length v) / realToFrac nsegs
  go v' | VG.null v' = []
        | VG.length v' < seglen = [v']
        | otherwise = let (v1,v2) = VG.splitAt seglen v'
                      in v1 : go v2

-- | Computes the product all elements
product :: Num a => Fold a a
product = Fold (*) 1 id (*)
{-# INLINABLE product #-}

-- | Like 'length', except with a more general 'Num' return value
genericLength :: Num b => Fold a b
genericLength = Fold (\n _ -> n + 1) 0 id (+)
{-# INLINABLE genericLength #-}

-- | Only process items that satisfy a predicate
filter :: (b -> Bool) -> Fold b c -> Fold b c
filter p (Fold step initial extract comb) = Fold step1 initial extract comb where
  step1 acc val =
    if p val then step acc val else acc

-- | Average
average :: Fractional a => Fold a a
average = (/) <$> sum <*> genericLength

-- | frequency
frequency :: Fold Bool Double
frequency = (/) <$> filter id genericLength <*> genericLength

-- | group Fold processing by an extracted key. Using a composite `a` gives a
-- pivot table
groupBy :: (Eq a, Ord a) => (b -> a) -> Fold b c -> Fold b (Map a c)
groupBy f (Fold step initial extract comb) = Fold step1 Map.empty (Map.map extract) comb1 where
  step1 mapacc val =
    let group = f val
    in case Map.lookup group mapacc of
        Nothing -> Map.insert group (step initial val) mapacc
        Just vOld -> Map.insert group (step vOld val) mapacc
  comb1 map1 map2 = Map.unionWith comb map1 map2

-- | Naive variance calculation
-- https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Na.C3.AFve_algorithm
-- we probably want to do this instead:
-- https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
variance :: Fractional a => Fold a a
variance = f <$> genericLength <*> sum <*> sumSqr where
  f (n::Int) sm smsqr = ((smsqr - (sm * sm))/realToFrac n) / (realToFrac $ n-1)
