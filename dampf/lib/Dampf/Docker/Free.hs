module Dampf.Docker.Free
  ( -- * Docker Interpreter
    runDockerT
  ) where

import           Control.Lens
import           Control.Monad                  (void)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Trans.Free       (iterT)
import           Data.Text                      (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           System.Process.Typed

import           Dampf.AppFile
import           Dampf.Docker.Types


-- Interpreter

runDockerT :: (MonadIO m) => DockerT m a -> m a
runDockerT = iterT dockerIter


dockerIter :: (MonadIO m) => DockerF (m a) -> m a
dockerIter (Build t i next) = interpBuild t i >> next
dockerIter (Rm c next)      = interpRm c >>= next
dockerIter (Run c s next)   = interpRun c s >> next
dockerIter (Stop c next)    = interpStop c >> next


interpBuild :: (MonadIO m) => Text -> FilePath -> m ()
interpBuild t i = do
    liftIO . putStrLn $ "Docker: Building " ++ i ++ ":" ++ show t
    void $ runProcess process
  where
    process = setStdin closed
        $ setStdout closed
        $ setStderr closed
        $ proc "docker" ["build", "-t", show t, i]


interpRm :: (MonadIO m) => Text -> m Text
interpRm c = do
    liftIO . putStrLn $ "Docker: Removing " ++ show c
    (_, o, _) <- readProcess process
    return . TL.toStrict $ TL.decodeUtf8 o
  where
    process = setStdin closed
        $ setStderr closed
        $ proc "docker" ["rm", show c]


interpRun :: (MonadIO m) => Text -> ContainerSpec -> m ()
interpRun c s = do
    liftIO . putStrLn $ "Docker: Running " ++ show c ++ " '" ++ cmd ++ "'"
    void $ runProcess process
  where
    cmd     = s ^. command . non ""
    ports   = concatMap (\x -> ["-p", show x ++ ":" ++ show x])
        (s ^. expose . non [])

    process = setStdin closed
        $ setStdout closed
        $ setStderr closed
        $ proc "docker" args

    args = concat [
        [ "run", "-d", "--restart=always", "--net=host", "--name=" ++ show c]
        , ports
        , [s ^. image]
        , words cmd
        ]


interpStop :: (MonadIO m) => Text -> m ()
interpStop c = do
    liftIO . putStrLn $ "Docker: Stopping " ++ show c
    void $ runProcess process
  where
    process = setStdin closed
        $ setStdout closed
        $ setStderr closed
        $ proc "docker" ["stop", show c]

