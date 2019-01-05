{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Dampf.Docker.Free
  where

import           Control.Lens
import           Control.Monad                  (void, (<=<))
import           Control.Monad.Catch            (MonadCatch, onException)
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

import           Dampf.Docker.Types
import           Dampf.Docker.Args.Network (createNet)
import           Dampf.Docker.Args 
import           Dampf.Types

-- Interpreter

runDockerT :: (MonadIO m, MonadCatch m) => DockerT (DampfT m) a -> DampfT m a
runDockerT = iterT dockerIter


dockerIter 
  :: (MonadIO m, MonadCatch m) 
  => DockerF (DampfT m a) 
  -> DampfT m a

dockerIter = \case
  Build t i next -> interpBuild t i >> next
  Rm c next      -> interpRm c >>= next
  RmMany cs next -> interpRmMany cs >>= next
  Run d c s next -> do
    onException 
      (interpRun d c s >>= next) 
      (interpRm  c)

  Exec c s next     -> interpExec c s >>= next
  Stop c next       -> interpStop c >> next
  StopMany cs next  -> interpStopMany cs >> next
  Pull n next       -> interpPull n >> next
  RunWith f n spec next -> 
    onException
      (interpRunWith f n spec >>= next)
      (interpRm n)

  NetworkCreate net next -> 
    onException
      (interp (defCreateArg net) >> next) 
      (interpNetworkRM [net])

  NetworkCreateWith args next -> 
    onException
      (interp args >> next)
      (interpNetworkRM [args ^. createNet])

  NetworkConnect net cont next      -> interp (defConnectArg net cont) >> next
  NetworkConnectWith args next      -> interp args >> next
  NetworkDisconnect n s next        -> interpNetworkDisconnect n s >> next
  NetworkLs next                    -> interpNetworkLS >>= next
  NetworkRm nets next               -> interpNetworkRM nets >>= next
  NetworkInspect format nets next   -> interpNetworkInspect format nets >>= next
  Inspect format id' next           -> interpInspect format id' >>= next
  ContainerLS next                  -> interpContainerLS >>= next

interpBuild :: (MonadIO m) => Text -> FilePath -> DampfT m ()
interpBuild t i = do
    liftIO . putStrLn $ "Docker: Building " ++ i ++ ":" ++ show t
    void $ runDockerProcess ["build", "-t", T.unpack t, i]

interpRm :: (MonadIO m) => Text -> DampfT m Text
interpRm c = interpRmMany [c]

interpRmMany :: (MonadIO m) => [Text] -> DampfT m Text
interpRmMany cs = do
    liftIO . putStrLn $ "Docker: Removing " ++ T.unpack (T.intercalate ", " cs)
    readDockerProcess $ ["rm", "-f"] ++ fmap T.unpack cs

interpRun :: (MonadIO m, MonadCatch m) => Bool -> Text -> ContainerSpec -> DampfT m Text
interpRun True  = interpRunWith id
interpRun False = interpRunWith unDaemonize

interpRunWith :: (MonadIO m, MonadCatch m) => (RunArgs -> RunArgs) -> Text -> ContainerSpec -> DampfT m Text
interpRunWith f n spec = do
    args <- mkRunArgs n spec <&> f
    liftIO . putStrLn $ "Docker: Running "
        ++ args ^. name . to T.unpack ++ " '" ++ args ^. cmd . to T.unpack ++ "'"

    readDockerProcess . toArgs $ args

interpExec :: (MonadIO m, MonadCatch m) => Text -> Text -> DampfT m Text
interpExec conNm cmds = do
  readDockerProcess $ "exec":words (unpack cmds)


interpStop :: (MonadIO m) => Text -> DampfT m ()
interpStop c = interpStopMany [c]

interpStopMany :: (MonadIO m) => [Text] -> DampfT m ()
interpStopMany cs = do
    liftIO . putStrLn $ "Docker: Stopping " ++ T.unpack (T.intercalate ", " cs)
    runDockerProcess $ ["stop"] ++ fmap T.unpack cs ++ ["-t", "1"]

readDockerProcess :: MonadIO m => [String] -> DampfT m Text
readDockerProcess = (go' <=< readProcess_) . proc "docker"
  where de  = TL.toStrict . TL.decodeUtf8
        out o = liftIO (T.putStrLn $ "stdout: " <> de o) *> return (de o)
        err e = liftIO (T.putStrLn $ "stderr: " <> de e) *> return (de e)
        go  (o, e) = out o <* err e
        go' (o, _) = return . de $ o

runDockerProcess :: MonadIO m => [String] -> DampfT m ()
runDockerProcess = runProcess_ . proc "docker"

interp :: (MonadIO m, MonadCatch m, ToArgs arg) => arg -> DampfT m ()
interp = runDockerProcess . toArgs

interpPull :: (MonadIO m, MonadCatch m) => Text -> DampfT m ()
interpPull image_name = do
  liftIO . putStrLn $ "Docker: Pulling " ++ T.unpack image_name
  runDockerProcess ["pull", T.unpack image_name]

interpNetworkDisconnect :: (MonadIO m, MonadCatch m) =>
     Text
  -> ContainerSpec
  -> DampfT m ()
interpNetworkDisconnect netName spec = do
  liftIO . putStrLn $ "Docker: Diconnecting " ++ spec ^. image . _Text
  runDockerProcess ["network", "disconnect", T.unpack netName, spec ^. image . to T.unpack]

interpNetworkLS :: (MonadIO m, MonadCatch m) => DampfT m Text
interpNetworkLS = readDockerProcess ["network", "ls"]

interpNetworkRM :: (MonadIO m, MonadCatch m) => [Text] -> DampfT m Text
interpNetworkRM nets = do
  liftIO . putStrLn $ "Docker: Removing network " ++ T.unpack (T.intercalate ", " nets)
  readDockerProcess $ ["network", "rm"] ++ fmap T.unpack nets

interpNetworkInspect :: (MonadIO m, MonadCatch m) => Text -> [Text] -> DampfT m Text
interpNetworkInspect format nets = do
  liftIO . putStrLn $ "Docker: Inspecting network " ++ T.unpack (T.intercalate ", " nets)
  readDockerProcess $ ["network", "inspect", "-f", T.unpack format] ++ fmap T.unpack nets

interpContainerLS :: (MonadIO m, MonadCatch m) => DampfT m Text
interpContainerLS = readDockerProcess $ ["container", "list"]

interpInspect :: (MonadIO m, MonadCatch m) => Text -> Text -> DampfT m Text
interpInspect format id' = do
  liftIO . putStrLn $ "Docker: Inspecting " ++ T.unpack id'
  readDockerProcess $ ["inspect", "-f", T.unpack format, take 12 $ T.unpack id']
