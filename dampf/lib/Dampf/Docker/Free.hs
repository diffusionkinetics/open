{-# LANGUAGE OverloadedStrings #-}

module Dampf.Docker.Free
  ( -- * Docker Interpreter
    runDockerT
  ) where

import           Control.Lens
import           Control.Monad                  (void)
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
dockerIter (Build t i next) = interpBuild t i >> next
dockerIter (Rm c next)      = interpRm c >>= next
dockerIter (Run d c s next)   = interpRun d c s >> next
dockerIter (Stop c next)    = interpStop c >> next


interpBuild :: (MonadIO m) => Text -> FilePath -> DampfT m ()
interpBuild t i = do
    liftIO . putStrLn $ "Docker: Building " ++ i ++ ":" ++ show t
    void $ runDockerProcess ["build", "-t", T.unpack t, i]


interpRm :: (MonadIO m) => Text -> DampfT m Text
interpRm c = do
    liftIO . putStrLn $ "Docker: Removing " ++ T.unpack c
    (_, o, _) <- readProcess process
    return . TL.toStrict $ TL.decodeUtf8 o
  where
    process = {-setStdin closed
        . setStderr closed
        $ -} proc "docker" ["rm", T.unpack c]


interpRun :: (MonadIO m, MonadThrow m) => Bool -> Text -> ContainerSpec -> DampfT m ()
interpRun daemonise n spec = do
    args <- if daemonise then mkRunArgs n spec else mkRunArgsNonDaemon n spec 
    liftIO . putStrLn $ "Docker: Running "
        ++ T.unpack n ++ " '" ++ args ^. cmd . to T.unpack ++ "'"
    runDockerProcess $ toArgs args



interpStop :: (MonadIO m) => Text -> DampfT m ()
interpStop c = do
    liftIO . putStrLn $ "Docker: Stopping " ++ T.unpack c
    void $ runDockerProcess ["stop", T.unpack c]


runDockerProcess :: MonadIO m => [String] -> DampfT m ()
runDockerProcess args = do
  --liftIO $ putStrLn $ "$ docker "++unwords args
  void $ runProcess (proc "docker" args)
