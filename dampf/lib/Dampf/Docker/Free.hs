{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Dampf.Docker.Free
  where

import           Control.Lens
import           Control.Monad                  (void, (<=<))
import           Control.Monad.Catch            (MonadThrow)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Trans.Free       (iterT)
import           Data.Text                      (Text)
import           Data.Text.Lens
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
  Pull n next    -> interpPull n >> next
  RunWith f n spec next         -> interpRunWith f n spec >>= next
  NetworkCreate net next        -> interp (defCreateArg net) >> next
  NetworkCreateWith args next   -> interp args >> next 
  NetworkConnect net cont next  -> interp (defConnectArg net cont) >> next
  NetworkConnectWith args next  -> interp args >> next
  NetworkDisconnect n s next    -> interpNetworkDisconnect n s >> next
  NetworkLs next                -> interpNetworkLS >>= next
  NetworkRm nets next           -> interpNetworkRM nets >>= next
  NetworkInspect format nets next      -> interpNetworkInspect format nets >>= next
  Inspect format id' next       -> interpInspect format id' >>= next
  ContainerLS next              -> interpContainerLS >>= next


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
interpRun = interpRunWith id


interpRunWith :: (MonadIO m, MonadThrow m) => (RunArgs -> RunArgs) -> Text -> ContainerSpec -> DampfT m Text
interpRunWith f n spec = do
    args <- mkRunArgs n spec <&> f
    liftIO . putStrLn $ "Docker: Running "
        ++ args ^. name . to T.unpack ++ " '" ++ args ^. cmd . to T.unpack ++ "'"
    res <- readDockerProcess . toArgs $ args
    liftIO . putStrLn . T.unpack $ res 
    return res


interpStop :: (MonadIO m) => Text -> DampfT m ()
interpStop c = do
    liftIO . putStrLn $ "Docker: Stopping " ++ T.unpack c
    runDockerProcess ["stop", T.unpack c]


readDockerProcess :: MonadIO m => [String] -> DampfT m Text
readDockerProcess = fmap (TL.toStrict . TL.decodeUtf8 . fst) . readProcess_ . proc "docker"

runDockerProcess :: MonadIO m => [String] -> DampfT m ()
runDockerProcess args = do
    liftIO . putStrLn $ "$ docker "++unwords args
    runProcess_ (proc "docker" args)

interp :: (MonadIO m, MonadThrow m, ToArgs arg) => arg -> DampfT m ()
interp = runDockerProcess . toArgs

interpPull :: (MonadIO m, MonadThrow m) => Text -> DampfT m ()
interpPull name = do
  liftIO . putStrLn $ "Docker: Pulling " ++ T.unpack name
  runDockerProcess ["pull", T.unpack name]

interpNetworkDisconnect :: (MonadIO m, MonadThrow m) => 
     Text 
  -> ContainerSpec 
  -> DampfT m ()
interpNetworkDisconnect netName spec = do
  liftIO . putStrLn $ "Docker: Diconnecting " ++ spec ^. image . _Text
  runDockerProcess ["network", "disconnect", T.unpack netName, spec ^. image . to T.unpack]

interpNetworkLS :: (MonadIO m, MonadThrow m) => DampfT m Text
interpNetworkLS = do
  res <- readDockerProcess ["network", "ls"]
  liftIO . putStrLn . T.unpack $ res
  return res

interpNetworkRM :: (MonadIO m, MonadThrow m) => [Text] -> DampfT m Text
interpNetworkRM nets = do
  liftIO . putStrLn $ "Docker: Removing " ++ T.unpack (T.intercalate ", " nets)
  readDockerProcess $ ["network", "rm"] ++ fmap T.unpack nets

interpNetworkInspect :: (MonadIO m, MonadThrow m) => Text -> [Text] -> DampfT m Text
interpNetworkInspect format nets = do
  liftIO . putStrLn $ "Docker: Inspecting network " ++ T.unpack (T.intercalate ", " nets)
  readDockerProcess $ ["network", "inspect", "-f", T.unpack format] ++ fmap T.unpack nets

interpContainerLS :: (MonadIO m, MonadThrow m) => DampfT m Text
interpContainerLS = do
  res <- readDockerProcess $ ["container", "list"]
  liftIO . putStrLn . T.unpack $ res
  return res

interpInspect :: (MonadIO m, MonadThrow m) => Text -> Text -> DampfT m Text
interpInspect format id' = do
  liftIO . putStrLn $ "Docker: Inspecting " ++ T.unpack id'
  readDockerProcess $ ["inspect", "-f", T.unpack format, take 12 $ T.unpack id']
