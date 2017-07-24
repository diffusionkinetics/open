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
import           Dampf.Internal.Docker.Args
import           Dampf.Types


-- Interpreter

runDockerT :: (MonadIO m, MonadThrow m) => DockerT (DampfT m) a -> DampfT m a
runDockerT = iterT dockerIter


dockerIter :: (MonadIO m, MonadThrow m) => DockerF (DampfT m a) -> DampfT m a
dockerIter (Build t i next) = interpBuild t i >> next
dockerIter (Rm c next)      = interpRm c >>= next
dockerIter (Run c s next)   = interpRun c s >> next
dockerIter (Stop c next)    = interpStop c >> next


interpBuild :: (MonadIO m) => Text -> FilePath -> DampfT m ()
interpBuild t i = do
    liftIO . putStrLn $ "Docker: Building " ++ i ++ ":" ++ show t
    void $ runProcess process
  where
    process = setStdin closed
        . setStdout closed
        . setStderr closed
        $ proc "docker" ["build", "-t", show t, i]


interpRm :: (MonadIO m) => Text -> DampfT m Text
interpRm c = do
    liftIO . putStrLn $ "Docker: Removing " ++ show c
    (_, o, _) <- readProcess process
    return . TL.toStrict $ TL.decodeUtf8 o
  where
    process = setStdin closed
        . setStderr closed
        $ proc "docker" ["rm", show c]


interpRun :: (MonadIO m, MonadThrow m) => Text -> ContainerSpec -> DampfT m ()
interpRun n spec = do
    args <- mkRunArgs n spec
    liftIO . putStrLn $ "Docker: Running "
        ++ T.unpack n ++ " '" ++ args ^. cmd . to T.unpack ++ "'"

    void . runProcess . process $ toArgs args
  where
    process = setStdin closed
        . setStdout closed
        . setStderr closed
        . proc "docker"


interpStop :: (MonadIO m) => Text -> DampfT m ()
interpStop c = do
    liftIO . putStrLn $ "Docker: Stopping " ++ show c
    void $ runProcess process
  where
    process = setStdin closed
        . setStdout closed
        . setStderr closed
        $ proc "docker" ["stop", show c]

