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
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|), (><))
import Data.List (foldl1')
import Data.Map.Strict (Map)
import Data.Profunctor
import Control.Monad.Par
import qualified Control.Foldl as L
import Data.List (maximumBy)
import Data.Ord (comparing)

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
import qualified Prelude

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

toFoldl :: Fold a b -> L.Fold a b
toFoldl (Fold step initial extract _) = L.Fold step initial extract

sum :: Num a => Fold a a
sum = Fold (+) 0 id (+)
{-# INLINABLE sum #-}

sumSqr :: Num a => Fold a a
sumSqr = premap sq sum where sq x = x * x

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
frequencyTrue :: Fold Bool Double
frequencyTrue = (/) <$> filter id genericLength <*> genericLength

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
  f (n::Int) sm smsqr = (smsqr - (sm * sm)/realToFrac n) / (realToFrac $ n-1)

-- | two-pass variance, for testing accuracy
twoPassVariance :: [Double] -> Double
twoPassVariance xs =
  let n = realToFrac $ Prelude.length xs
      mn = Prelude.sum xs / n
      devs = map (\x-> (x - mn)^2) xs
  in Prelude.sum devs / (n-1)

rmse :: Floating a => Fold (a,a) a
rmse = pure sqrt <*> premap (\(x,y)-> (x-y)*(x-y)) average

mode :: (Eq a, Ord a) => Fold a a
mode = Fold step Map.empty findMaxIx comb1 where
  step mapacc x = Map.insertWith (+) x (1::Int) mapacc
  findMaxIx mp = fst $ maximumBy (comparing snd) $ Map.toList mp
  comb1 mp1 mp2 = Map.unionWith (+) mp1 mp2

occurrences :: (Eq a, Ord a) => Fold a (Map a Int)
occurrences = Fold step Map.empty id comb1 where
  step mapacc x = Map.insertWith (+) x 1 mapacc
  comb1 mp1 mp2 = Map.unionWith (+) mp1 mp2

--http://wis.kuleuven.be/stat/robust/papers/publications-1990/rousseeuwbassett-remedian-jasa-1990.pdf
remedian :: (Eq a, Ord a, Show a) => Int -> Fold a a
remedian nbuf' = Fold step ini final comb where
  nbuf = if odd nbuf' then nbuf' else nbuf' +1
  ini = Pair Seq.empty (Pair Seq.empty (0::Int))
  step (Pair ms (Pair seqacc nacc)) x
    | nacc < nbuf = Pair ms (Pair (x <| seqacc) (nacc+1))
    | otherwise = let m = oddMedianS seqacc
                  in Pair (m <| ms) (Pair (Seq.singleton x) 1)
  final (Pair ms (Pair _ 0)) = oddMedianS ms
  final (Pair ms (Pair leftover _)) = oddMedianS $ oddMedianS leftover <| ms
  comb (Pair ms1 (Pair seqacc1 nacc1))
       (Pair ms2 (Pair seqacc2 nacc2))
       = Pair (ms1 >< ms2) (Pair (seqacc1 >< seqacc2) (nacc1+nacc2))

medianL :: (Ord a, Num a, Fractional a) => [a] -> a
medianL = medianS . Seq.fromList

medianS :: (Ord a, Num a, Fractional a) => Seq a -> a
medianS s | odd len = sorted `Seq.index` (len `div` 2)
          | otherwise = av2 (sorted `Seq.index` (len `div` 2 - 1)) (sorted `Seq.index` (len `div` 2))
  where sorted = Seq.unstableSort s
        len = Seq.length s
        av2 x y = (x+y)/2

oddMedianS :: (Ord a) => Seq a -> a
oddMedianS s = sorted `Seq.index` (len `div` 2)
  where sorted = Seq.unstableSort s
        len = Seq.length s
