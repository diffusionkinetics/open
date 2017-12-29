{-# LANGUAGE DeriveFunctor #-}

module Dampf.Docker.Types where
--
--
--   ( -- * Docker Monad
--     DockerT
--   , DockerF(..)
--     -- * Docker Combinators
--   , build
--   , rm
--   , run
--   , stop
--   ) where

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
    | RmMany [Text] (Text -> next)
    | Run Bool Text ContainerSpec (Text -> next)
    | RunWith (RunArgs -> RunArgs) Text ContainerSpec (Text -> next)
    | Stop Text next
    | StopMany [Text] next
    | Pull Text next
    | NetworkCreate Text next
    | NetworkCreateWith CreateArgs next
    | NetworkConnect Text Text next
    | NetworkConnectWith ConnectArgs next
    | NetworkDisconnect Text ContainerSpec next
    | NetworkLs (Text -> next)
    | NetworkRm [Text] (Text -> next)
    | NetworkInspect Text [Text] (Text -> next)
    | Inspect Text Text (Text -> next)
    | ContainerLS (Text -> next)
    deriving (Functor)


-- Docker Combinators

build :: (MonadIO m) => Text -> FilePath -> DockerT m ()
build t i = liftF (Build t i ())


rm :: (MonadIO m) => Text -> DockerT m Text
rm c = liftF (Rm c id)

rmMany :: (MonadIO m) => [Text] -> DockerT m Text
rmMany cs = liftF (RmMany cs id)

run :: (MonadIO m) => Bool -> Text -> ContainerSpec -> DockerT m Text
run d c s = liftF (Run d c s id)

runWith :: (MonadIO m) => (RunArgs -> RunArgs) -> Text -> ContainerSpec -> DockerT m Text
runWith f n spec = liftF (RunWith f n spec id)

stop :: (MonadIO m) => Text -> DockerT m ()
stop c = liftF (Stop c ())

stopMany :: (MonadIO m) => [Text] -> DockerT m ()
stopMany cs = liftF (StopMany cs ())

netCreate :: (MonadIO m) => Text -> DockerT m ()
netCreate net = liftF (NetworkCreate net ())

netCreateWith :: (MonadIO m) => CreateArgs -> DockerT m ()
netCreateWith args = liftF (NetworkCreateWith args ())

netConnect :: (MonadIO m) => Text -> Text -> DockerT m ()
netConnect net cont = liftF (NetworkConnect net cont ())

netConnectWith :: (MonadIO m) => ConnectArgs -> DockerT m ()
netConnectWith args = liftF (NetworkConnectWith args ())

netDisconnect :: (MonadIO m) => Text -> ContainerSpec -> DockerT m ()
netDisconnect net spec = liftF (NetworkDisconnect net spec ())

netLS :: (MonadIO m) => DockerT m Text
netLS = liftF (NetworkLs id)

netRM :: (MonadIO m) => [Text] -> DockerT m Text
netRM nets = liftF (NetworkRm nets id)

netInspect :: (MonadIO m) => Text -> [Text] -> DockerT m Text
netInspect format nets = liftF (NetworkInspect format nets id)

inspect :: (MonadIO m) => Text -> Text -> DockerT m Text
inspect format id' = liftF (Inspect format id' id)

pull :: (MonadIO m) => Text -> DockerT m ()
pull name = liftF (Pull name ())

containerLS :: (MonadIO m) => DockerT m Text
containerLS = liftF (ContainerLS id)
