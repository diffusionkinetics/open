module Fuml.HNN 
    where

import Fuml.Core
import qualified Data.Vector.Storable as V
import Numeric.LinearAlgebra
import Data.Bifunctor
import Data.Set (Set)
import Data.Map (Map)
import Data.List (nub)
import Data.Traversable (mapAccumL)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import AI.HNN.FF.Network

data Learning a = Learning (ActivationFunction a) (Network a -> Samples a -> Network a)

-- scaling factors for  input     output
--                      |         |
data Scaling a = Scale (Vector R) a

sigmoidNTimes n lrate = Learning sigmoid (trainNTimes n lrate sigmoid sigmoid') :: Learning R
tanhNTimes n lrate = Learning tanh (trainNTimes n lrate tanh tanh') :: Learning R

sigmoidErrorBelow err lrate = Learning sigmoid (trainUntilErrorBelow err lrate sigmoid sigmoid') :: Learning R
tanhErrorBelow err lrate = Learning tanh (trainUntilErrorBelow err lrate tanh tanh') :: Learning R

oneOutput :: 
     [Int]      -- hidden layers
  -> Learning R
  -> Maybe (Scaling R)
  -> Supervisor IO R (Scaling R, Network R) R
oneOutput layers (Learning act train) mbSc = Supervisor sup where
  sup Nothing theData = do
    let nfeatures = V.length . fst . head $ theData
    net <- flip train (scaled sc theData) <$> createNetwork nfeatures layers 1
    predWith sc net act

  sup (Just (sc', net)) theData =
    let new_net = train net (scaled sc' theData)
     in predWith sc' new_net act

  sc = case mbSc of
    Nothing -> Scale 1 1
    Just x -> x

  scaled (Scale i o) theData = bimap (* i) (V.singleton . (* o)) <$> theData
  predWith sc@(Scale i o) net act = return . Predict (sc, net) $ (/ o) . V.head . output net act . (* i)

multipleOutputs ::
     [Int]      -- hidden layers
  -> Learning R
  -> Maybe (Scaling (Vector R))
  -> Supervisor IO (Vector R) (Scaling (Vector R), Network R) (Vector R)
multipleOutputs layers (Learning act train) mbSc = Supervisor sup where
  sup Nothing theData = do
    let nfeatures = V.length . fst . head $ theData
        noutputs  = V.length . snd . head $ theData
    net <- flip train (scaled sc theData) <$> createNetwork nfeatures layers noutputs
    predWith sc net act

  sup (Just (sc', net)) theData = do
    let new_net = train net (scaled sc' theData)
     in predWith sc' new_net act

  sc = case mbSc of 
    Nothing -> Scale 1 1
    Just x -> x

  scaled (Scale i o) theData = bimap (* i) (* o) <$> theData
  predWith sc@(Scale i o) net act = return . Predict (sc, net) $ (/ o) . output net act . (* i)

oneClass :: 
     [Int]              -- hidden layers
  -> Learning R
  -> Maybe (Vector R)   -- input scaling coefficients
  -> Supervisor IO Bool (Scaling R, Network R) R
oneClass layers learning mb_scales = go (oneOutput layers learning scales)
  where go (Supervisor sup) = Supervisor $ \mbP -> sup mbP . map (fmap $ fromIntegral . fromEnum)
        
        scales = case mb_scales of
          Nothing -> Just $ Scale 1 1
          Just x  -> Just $ Scale x 1


multiClass :: Ord o =>
     [Int]              -- hidden layers 
  -> Learning R
  -> Maybe (Vector R)   -- input scaling coefficients
  -> Supervisor IO o ([o], Vector R, Network R) (Map o R)
multiClass layers (Learning act train) mbCoefs = Supervisor sup where
  sup Nothing theData = do
    let nfeatures = V.length . fst . head $ theData
        noutputs = length keys
        keys = nub . map snd $ theData

    net <- flip train (convert inputCoefs keys theData) <$> createNetwork nfeatures layers noutputs
    predWith inputCoefs keys net

  sup (Just (keys, inputCoefs, net)) theData =
    let new_net = train net (convert inputCoefs keys theData)
     in predWith inputCoefs keys new_net 

  inputCoefs = case mbCoefs of 
    Nothing -> 1
    Just x -> x

  convert i keys theData = bimap (* i) (toVector keys) <$> theData
  predWith inputCoefs keys net = return (Predict (keys, inputCoefs, net) (toClass keys . output net act . (* inputCoefs)))

  toClass :: Ord o => [o] -> Vector R -> Map o R
  toClass vo = Map.fromList . zip vo . V.toList 

  toVector :: Ord o => [o] -> o -> Vector R
  toVector vo o = vector $ map go vo
    where go x | x == o = 1 | otherwise = 0

overlappingClasses :: Ord o =>
     [Int]               -- hidden layers
  -> Learning R   
  -> Maybe (Vector R)    -- input scaling coefficients
  -> Supervisor IO (Set o) ([o], Vector R, Network R) (Map o Double)
overlappingClasses layers (Learning act train) mbCoefs = Supervisor sup where
  sup Nothing theData = do
    let nfeatures = V.length . fst . head $ theData
        noutputs = length keys
        keys = nub . concat . map (Set.elems . snd) $ theData

    net <- flip train (convert inputCoefs keys theData) <$> createNetwork nfeatures layers noutputs
    predWith inputCoefs keys net

  sup (Just (keys, inputCoefs, net)) theData = 
    let new_net = train net (convert inputCoefs keys theData)

     in predWith inputCoefs keys new_net

  inputCoefs = case mbCoefs of 
    Nothing -> 1
    Just x -> x

  convert i keys theData = bimap (* i) (toVector' keys) <$> theData
  predWith inputCoefs keys net = return (Predict (keys, inputCoefs, net) (toClasses keys . output net act . (* inputCoefs)))

  toClasses :: Ord o => [o] -> Vector R -> Map o R
  toClasses vo = Map.fromList . zip vo . V.toList

  toVector' :: Ord o => [o] -> Set o -> Vector R
  toVector' vo so = vector . snd $ mapAccumL accum 0 vo
    where accum a b = (a+1, if Set.member b so then 1 else 0)
