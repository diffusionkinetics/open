{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TupleSections #-}

module Fuml.Core where

import qualified Data.Vector.Storable as VS
import Numeric.LinearAlgebra
import Data.List (nub)
import Lens.Micro

data Predict p a = Predict
  { model :: p
  , predict :: Vector Double -> a
  }

instance Functor (Predict p) where
  fmap f (Predict m predf) = Predict m (f . predf)

newtype Supervisor m o p a = Supervisor { runSupervisor :: Maybe p -> [(Vector Double, o)] -> m (Predict p a) }

instance Functor m => Functor (Supervisor m o p) where
  fmap f (Supervisor sf) = Supervisor $ \mp d -> fmap (fmap f) $ sf mp d


--oneVsRest :: Eq a => Supervisor Bool p -> Supervisor a (Multiclass a p)
oneVsRest :: (Monad m, Eq a, Functor m) => Supervisor m Bool p b -> Supervisor m a [(a,p)] [(a,b)]
oneVsRest subsuper  = Supervisor $ \_ theData -> do
  let classes = nub $ map snd theData
      boolize c (v, c1) = (v, c == c1)
      train c = fmap (c,) $ runSupervisor subsuper Nothing $ map (boolize c) theData
  models <-  mapM train classes
  return $ Predict (map (over _2 model) models) $ \v -> map (\(c,pr) -> (c, predict pr v)) models


(~~) :: (a -> o) -> [a -> Double] -> [a] -> [(Vector Double, o)]
fy ~~ fxs = let f w = (VS.fromList (map ($w) fxs) , fy w)
            in map f

--prepare :: (a -> o) -> [a -> Double] -> [a] -> [(Vector Double, o)]
--prepare fo fxs d = fo ~~ fxs $ d
