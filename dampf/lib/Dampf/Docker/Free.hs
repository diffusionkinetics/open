{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Dampf.Docker.Free
  where

import           Control.Lens
import           Control.Monad                  (void, (<=<))
import           Control.Monad.Catch            (MonadThrow)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Trans.Free       (iterT)
import           Data.Text                      (Text, unpack)
import           Data.Text.Lens
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.IO as T
import           Data.Monoid ((<>))
import           System.Process.Typed
import           System.Exit
import           Dampf.Docker.Types
import           Dampf.Docker.Args
import           Dampf.Types

-- Interpreter

runDockerT :: (MonadIO m, MonadThrow m) => DockerT (DampfT m) a -> DampfT m a
runDockerT = iterT dockerIter


dockerIter :: (MonadIO m, MonadThrow m) => DockerF (DampfT m a) -> DampfT m a
dockerIter = \case
  Build t i next -> interpBuild t i >> next
  Rm c next      -> interpRm c >> next
  RmMany cs next -> interpRmMany cs >> next
  Run d c s next -> interpRun d c s >>= next
  Exec c s next -> interpExec c s >>= next
  Stop c next    -> interpStop c >> next
  StopMany cs next -> interpStopMany cs >> next
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
    void $ runDockerProcess_ ["build", "-t", T.unpack t, i]

interpRm :: (MonadIO m) => Text -> DampfT m ()
interpRm c = interpRmMany [c]

interpRmMany :: (MonadIO m) => [Text] -> DampfT m ()
interpRmMany cs = do
    liftIO . putStrLn $ "Docker: Removing " ++ T.unpack (T.intercalate ", " cs)
    void $ runDockerProcess  $ ["rm", "-f"] ++ fmap T.unpack cs

interpRun :: (MonadIO m, MonadThrow m) => Bool -> Text -> ContainerSpec -> DampfT m Text
interpRun True  = interpRunWith id
interpRun False = interpRunWith unDaemonize

interpRunWith :: (MonadIO m, MonadThrow m) => (RunArgs -> RunArgs) -> Text -> ContainerSpec -> DampfT m Text
interpRunWith f n spec = do
    args <- mkRunArgs n spec <&> f
    liftIO . putStrLn $ "Docker: Running "
        ++ args ^. name . to T.unpack ++ " '" ++ args ^. cmd . to T.unpack ++ "'"
    if args ^. interactive
      then do
        runDockerProcess_ . toArgs $ args
        return ""
      else do
        res <- readDockerProcess . toArgs $ args
        liftIO . T.putStrLn $ res
        return res

interpExec :: (MonadIO m, MonadThrow m) => Text -> Text -> DampfT m Text
interpExec conNm cmds = do
  readDockerProcess $ "exec":words (unpack cmds)


interpStop :: (MonadIO m) => Text -> DampfT m ()
interpStop c = interpStopMany [c]

interpStopMany :: (MonadIO m) => [Text] -> DampfT m ()
interpStopMany cs = do
    liftIO . putStrLn $ "Docker: Stopping " ++ T.unpack (T.intercalate ", " cs)
    void $ runDockerProcess $ ["stop"] ++ fmap T.unpack cs ++ ["-t", "1"]

readDockerProcess :: MonadIO m => [String] -> DampfT m Text
readDockerProcess = (go' <=< readProcess_) . proc "docker"
  where de  = TL.toStrict . TL.decodeUtf8
        out o = liftIO (T.putStrLn $ "stdout: " <> de o) *> return (de o)
        err e = liftIO (T.putStrLn $ "stderr: " <> de e) *> return (de e)
        go  (o, e) = out o <* err e
        go' (o, _) = return . de $ o

runDockerProcess_ :: MonadIO m => [String] -> DampfT m ()
runDockerProcess_ = runProcess_ . proc "docker"

runDockerProcess :: MonadIO m => [String] -> DampfT m ExitCode
runDockerProcess = runProcess . proc "docker"

interp :: (MonadIO m, MonadThrow m, ToArgs arg) => arg -> DampfT m ()
interp = runDockerProcess_ . toArgs

interpPull :: (MonadIO m, MonadThrow m) => Text -> DampfT m ()
interpPull image_name = do
  liftIO . putStrLn $ "Docker: Pulling " ++ T.unpack image_name
  runDockerProcess_ ["pull", T.unpack image_name]

interpNetworkDisconnect :: (MonadIO m, MonadThrow m) =>
     Text
  -> ContainerSpec
  -> DampfT m ()
interpNetworkDisconnect netName spec = do
  liftIO . putStrLn $ "Docker: Diconnecting " ++ spec ^. image . _Text
  runDockerProcess_ ["network", "disconnect", T.unpack netName, spec ^. image . to T.unpack]

interpNetworkLS :: (MonadIO m, MonadThrow m) => DampfT m Text
interpNetworkLS = readDockerProcess ["network", "ls"]

interpNetworkRM :: (MonadIO m, MonadThrow m) => [Text] -> DampfT m Text
interpNetworkRM nets = do
  liftIO . putStrLn $ "Docker: Removing network " ++ T.unpack (T.intercalate ", " nets)
  readDockerProcess $ ["network", "rm"] ++ fmap T.unpack nets

interpNetworkInspect :: (MonadIO m, MonadThrow m) => Text -> [Text] -> DampfT m Text
interpNetworkInspect format nets = do
  liftIO . putStrLn $ "Docker: Inspecting network " ++ T.unpack (T.intercalate ", " nets)
  readDockerProcess $ ["network", "inspect", "-f", T.unpack format] ++ fmap T.unpack nets

interpContainerLS :: (MonadIO m, MonadThrow m) => DampfT m Text
interpContainerLS = readDockerProcess $ ["container", "list"]

interpInspect :: (MonadIO m, MonadThrow m) => Text -> Text -> DampfT m Text
interpInspect format id' = do
  liftIO . putStrLn $ "Docker: Inspecting " ++ T.unpack id'
  readDockerProcess $ ["inspect", "-f", T.unpack format, take 12 $ T.unpack id']
