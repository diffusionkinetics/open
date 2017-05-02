{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TupleSections #-}

module Fuml.Core where

import qualified Data.Vector.Storable as VS
import Numeric.LinearAlgebra
import Data.List (nub)
import Lens.Micro
import Control.Monad.Identity

-- |The result of running a predictive model
data Predict p a = Predict
  { model :: p -- ^ the internal state of the model
  , predict :: Vector Double -> a -- ^ the prediction function
  }

instance Functor (Predict p) where
  fmap f (Predict m predf) = Predict m (f . predf)

newtype Supervisor m o p a
  = Supervisor { runSupervisor :: Maybe p
                               -> [(Vector Double, o)]
                               -> m (Predict p a) }

instance Functor m => Functor (Supervisor m o p) where
  fmap f (Supervisor sf) = Supervisor $ \mp d -> fmap (fmap f) $ sf mp d

-- | Helper function for running simple Supervisors in the Identity monad
runSupervisor' :: Supervisor Identity o p a -- ^ the 'Supervisor' value
               -> [(Vector Double, o)] -- ^ the dataset
               -> Predict p a -- ^ the 'Predict' value
runSupervisor' (Supervisor sf) = runIdentity . sf Nothing

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
