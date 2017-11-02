{-# LANGUAGE OverloadedStrings #-}

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
import           Dampf.Docker.Args.Run (RunArgs)
import           Dampf.Types


-- Interpreter

runDockerT :: (MonadIO m, MonadThrow m) => DockerT (DampfT m) a -> DampfT m a
runDockerT = iterT dockerIter


dockerIter :: (MonadIO m, MonadThrow m) => DockerF (DampfT m a) -> DampfT m a
dockerIter (Build t i next) = interpBuild t i >> next
dockerIter (Rm c next)      = interpRm c >>= next
dockerIter (Run c s next)   = interpRun c s >>= next
dockerIter (Stop c next)    = interpStop c >> next
dockerIter (RunWith args next)   = interpRunWith args >>= next
dockerIter (NetworkCreate n next) = interpNetworkCreate n >> next
dockerIter (NetworkConnect n s next) = interpNetworkConnect n s >> next
dockerIter (NetworkDisconnect n s next) = interpNetworkDisconnect n s >> next
dockerIter (NetworkLs next) = interpNetworkLS >>= next
dockerIter (NetworkRm nets next) = interpNetworkRM nets >>= next
dockerIter (NetworkInspect nets next) = interpNetworkInspect nets >>= next


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



interpNetworkCreate :: (MonadIO m, MonadThrow m) => Text -> DampfT m ()
interpNetworkCreate netName = do
  liftIO . putStrLn $ "Docker: Creating netNamework " ++ T.unpack netName
  runDockerProcess ["netNamework", "create", T.unpack netName]
  
interpNetworkConnect :: (MonadIO m, MonadThrow m) => Text -> ContainerSpec -> DampfT m ()
interpNetworkConnect netName spec = 
  runDockerProcess ["netNamework", "connect", T.unpack netName, spec ^. image . to T.unpack]
  
interpNetworkDisconnect :: (MonadIO m, MonadThrow m) => Text -> ContainerSpec -> DampfT m ()
interpNetworkDisconnect netName spec =
  runDockerProcess ["netNamework", "disconnect", T.unpack netName, spec ^. image . to T.unpack]

interpNetworkLS :: (MonadIO m, MonadThrow m) => DampfT m Text
interpNetworkLS = 
  readDockerProcess ["network", "ls"]

interpNetworkRM :: (MonadIO m, MonadThrow m) => [Text] -> DampfT m Text
interpNetworkRM nets = 
  readDockerProcess $ ["network", "rm"] ++ fmap T.unpack nets

interpNetworkInspect :: (MonadIO m, MonadThrow m) => [Text] -> DampfT m Text
interpNetworkInspect nets = 
  readDockerProcess $ ["network", "rm"] ++ fmap T.unpack nets
