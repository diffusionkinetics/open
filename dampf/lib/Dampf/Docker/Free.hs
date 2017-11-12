{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Dampf.Docker.Free
  ( -- * Docker Interpreter
    runDockerT
  ) where

import           Control.Lens
import           Control.Monad                  (void, (<=<))
import           Control.Monad.Catch            (MonadThrow)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Trans.Free       (iterT)
import           Data.Text                      (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           System.Process.Typed

import           Dampf.Docker.Types
import           Dampf.Docker.Args
import           Dampf.Types


-- Interpreter

runDockerT :: (MonadIO m, MonadThrow m) => DockerT (DampfT m) a -> DampfT m a
runDockerT = iterT dockerIter


dockerIter :: (MonadIO m, MonadThrow m) => DockerF (DampfT m a) -> DampfT m a
dockerIter = \case
  Build t i next -> interpBuild t i >> next
  Rm c next      -> interpRm c >>= next
  Run c s next   -> interpRun c s >>= next
  Stop c next    -> interpStop c >> next
  RunWith args next             -> interpRunWith args >>= next
  NetworkCreate net next        -> interp (defCreateArg net) >> next
  NetworkCreateWith args next   -> interp args >> next 
  NetworkConnect net cont next  -> interp (defConnectArg net cont) >> next
  NetworkConnectWith args next  -> interp args >> next
  NetworkDisconnect n s next    -> interpNetworkDisconnect n s >> next
  NetworkLs next                -> interpNetworkLS >>= next
  NetworkRm nets next           -> interpNetworkRM nets >>= next
  NetworkInspect nets next      -> interpNetworkInspect nets >>= next


interpBuild :: (MonadIO m) => Text -> FilePath -> DampfT m ()
interpBuild t i = do
    liftIO . putStrLn $ "Docker: Building " ++ i ++ ":" ++ show t
    void $ runDockerProcess ["build", "-t", T.unpack t, i]


interpRm :: (MonadIO m) => Text -> DampfT m Text
interpRm c = do
    liftIO . putStrLn $ "Docker: Removing " ++ T.unpack c
    liftIO $ putStrLn $ "$ docker rm "++T.unpack c
    readDockerProcess ["rm", T.unpack c]


interpRun :: (MonadIO m, MonadThrow m) => Text -> ContainerSpec -> DampfT m Text
interpRun n = interpRunWith <=< mkRunArgs n


interpRunWith :: (MonadIO m, MonadThrow m) => RunArgs -> DampfT m Text
interpRunWith args = do
    liftIO . putStrLn $ "Docker: Running "
        ++ args ^. name . to T.unpack ++ " '" ++ args ^. cmd . to T.unpack ++ "'"
    readDockerProcess . toArgs $ args


interpStop :: (MonadIO m) => Text -> DampfT m ()
interpStop c = do
    liftIO . putStrLn $ "Docker: Stopping " ++ T.unpack c
    runDockerProcess ["stop", T.unpack c]


readDockerProcess :: MonadIO m => [String] -> DampfT m Text
readDockerProcess = fmap (TL.toStrict . TL.decodeUtf8 . fst) . readProcess_ . proc "docker"

runDockerProcess :: MonadIO m => [String] -> DampfT m ()
runDockerProcess args = do
    liftIO $ putStrLn $ "$ docker "++unwords args
    runProcess_ (proc "docker" args)

-- 

interp :: (MonadIO m, MonadThrow m, ToArgs arg) => arg -> DampfT m ()
interp = runDockerProcess . toArgs

--

interpNetworkDisconnect :: (MonadIO m, MonadThrow m) => Text -> ContainerSpec -> DampfT m ()
interpNetworkDisconnect netName spec =
  runDockerProcess ["network", "disconnect", T.unpack netName, spec ^. image . to T.unpack]

interpNetworkLS :: (MonadIO m, MonadThrow m) => DampfT m Text
interpNetworkLS = 
  readDockerProcess ["network", "ls"]

interpNetworkRM :: (MonadIO m, MonadThrow m) => [Text] -> DampfT m Text
interpNetworkRM nets = 
  readDockerProcess $ ["network", "rm"] ++ fmap T.unpack nets

interpNetworkInspect :: (MonadIO m, MonadThrow m) => [Text] -> DampfT m Text
interpNetworkInspect nets = 
  readDockerProcess $ ["network", "rm"] ++ fmap T.unpack nets
