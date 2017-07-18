{-# LANGUAGE DeriveFunctor #-}

module Dampf.Docker.Types
  ( -- * Docker Monad
    DockerT
  , DockerF(..)
    -- * Docker Combinators
  , build
  , rm
  , run
  , stop
  ) where

import Control.Monad.IO.Class   (MonadIO)
import Control.Monad.Trans.Free (FreeT, liftF)
import Data.Text                (Text)

import Dampf.AppFile


-- Docker Monad

type DockerT = FreeT DockerF


data DockerF next
    = Build Text FilePath next
    | Rm Text (Text -> next)
    | Run Text ContainerSpec next
    | Stop Text next
    deriving (Functor)


-- Docker Combinators

build :: (MonadIO m) => Text -> FilePath -> DockerT m ()
build t i = liftF (Build t i ())


rm :: (MonadIO m) => Text -> DockerT m Text
rm c = liftF (Rm c id)


run :: (MonadIO m) => Text -> ContainerSpec -> DockerT m ()
run c s = liftF (Run c s ())


stop :: (MonadIO m) => Text -> DockerT m ()
stop c = liftF (Stop c ())

