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

import Dampf.Types
import Dampf.Docker.Args

-- Docker Monad

type DockerT = FreeT DockerF


data DockerF next
    = Build Text FilePath next
    | Rm Text (Text -> next)
    | Run Text ContainerSpec (Text -> next)
    | RunWith RunArgs (Text -> next)
    | Stop Text next
    | NetworkCreate Text next
    | NetworkCreateWith CreateArgs next
    | NetworkConnect Text Text next
    | NetworkConnectWith ConnectArgs next
    | NetworkDisconnect Text ContainerSpec next
    | NetworkLs (Text -> next)
    | NetworkRm [Text] (Text -> next)
    | NetworkInspect [Text] (Text -> next)
    deriving (Functor)


-- Docker Combinators

build :: (MonadIO m) => Text -> FilePath -> DockerT m ()
build t i = liftF (Build t i ())


rm :: (MonadIO m) => Text -> DockerT m Text
rm c = liftF (Rm c id)


run :: (MonadIO m) => Text -> ContainerSpec -> DockerT m Text
run c s = liftF (Run c s id)


runWith :: (MonadIO m) => RunArgs -> DockerT m Text
runWith args = liftF (RunWith args id)


stop :: (MonadIO m) => Text -> DockerT m ()
stop c = liftF (Stop c ())

netCreate :: (MonadIO m) => Text -> DockerT m ()
netCreate net = liftF (NetworkCreate net ())

netConnect :: (MonadIO m) => Text -> Text -> DockerT m ()
netConnect net cont = liftF (NetworkConnect net cont ())

netDisconnect :: (MonadIO m) => Text -> ContainerSpec -> DockerT m ()
netDisconnect net spec = liftF (NetworkDisconnect net spec ())

netLS :: (MonadIO m) => DockerT m Text
netLS = liftF (NetworkLs id)

netRM :: (MonadIO m) => [Text] -> DockerT m Text
netRM nets = liftF (NetworkRm nets id)

netInspect :: (MonadIO m) => [Text] -> DockerT m Text
netInspect nets = liftF (NetworkInspect nets id)
