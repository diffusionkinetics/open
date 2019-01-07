{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

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
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid ((<>))
import           System.Process.Typed
import           System.Exit (ExitCode(..))

import           Dampf.Docker.Types
import           Dampf.Docker.Args.Network (createNet)
import           Dampf.Docker.Args hiding (net)
import           Dampf.Types

-- Interpreter

runDockerT 
  :: MonadIO m
  => MonadCatch m 
  => DockerT (DampfT m) a 
  -> DampfT m a

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

interpBuild :: MonadIO m => Text -> FilePath -> m ()
interpBuild t i = liftIO $ do
    putStrLn $ 
      "Docker: Building " ++ i ++ ":" ++ show t

    void . readDockerProcess $ 
      ["build", "-t", T.unpack t, i]


interpRm :: MonadIO m => Text -> DampfT m Text
interpRm c = interpRmMany [c]


interpRmMany :: MonadIO m => [Text] -> m Text
interpRmMany cs = liftIO $ do
    putStrLn $
      "Docker: Removing " ++ T.unpack (T.intercalate ", " cs)

    readDockerProcess $ 
      ["rm", "-f"] ++ fmap T.unpack cs


interpRun 
  :: MonadIO m 
  => MonadCatch m 
  => Bool 
  -> Text 
  -> ContainerSpec 
  -> DampfT m Text

interpRun True  = interpRunWith id
interpRun False = interpRunWith unDaemonize


interpRunWith 
  :: MonadIO m => MonadCatch m 
  => (RunArgs -> RunArgs) 
  -> Text 
  -> ContainerSpec 
  -> DampfT m Text

interpRunWith argsTweak n spec = do
    args <- mkRunArgs n spec <&> argsTweak
    liftIO . putStrLn $ "Docker: Running " 
      <> show (args ^. name <> args ^. cmd)

    readDockerProcess . toArgs $ args


interpExec 
  :: MonadIO m 
  => MonadCatch m 
  => Text 
  -> Text 
  -> m Text
interpExec conNm cmds = do
  readDockerProcess $ "exec" : words (unpack cmds)


interpStop :: MonadIO m => Text -> DampfT m ()
interpStop c = interpStopMany [c]


interpStopMany :: MonadIO m => [Text] -> m ()
interpStopMany cs = liftIO $ do
    putStrLn $ 
      "Docker: Stopping " ++ T.unpack (T.intercalate ", " cs)

    void . readDockerProcess $
      ["stop"] ++ fmap T.unpack cs ++ ["-t", "1"]


readDockerProcess 
  :: forall m. MonadIO m 
  => [String] 
  -> m Text

readDockerProcess args = (go <=< readProcess) . proc "docker" $ args `seq` args
  where 
    dec = TL.toStrict . TL.decodeUtf8
    spread = (<> " ")

    go :: (ExitCode, BL.ByteString, BL.ByteString) -> m Text
    go (ExitSuccess, out, _) = liftIO $ do 
      -- BL.putStrLn ("stdout: " <> out)
      (return . dec) out

    go (ExitFailure code, out, err) = liftIO $ do
      BL.putStr $ 
           "\n[FAIL]: docker " 
        <> (BL.pack . foldMap spread) args
        <> "\n error code: " <> (BL.pack . show) code
        <> "\n stderr: " <> err
      (return . dec) out


interp 
  :: MonadIO m 
  => MonadCatch m 
  => ToArgs arg 
  => arg 
  -> m ()
interp = void . readDockerProcess . toArgs


interpPull 
  :: MonadIO m 
  => MonadCatch m 
  => Text 
  -> m ()

interpPull imageName = liftIO $ do
  putStrLn $ 
    "Docker: Pulling " ++ T.unpack imageName

  (void . readDockerProcess) [ "pull", T.unpack imageName ]


interpNetworkDisconnect 
  :: MonadIO m 
  => MonadCatch m 
  => Text
  -> ContainerSpec
  -> m ()

interpNetworkDisconnect netName spec = liftIO $ do
  putStrLn $ 
    "Docker: Diconnecting " ++ spec ^. image . _Text

  (void . readDockerProcess) 
    [ "network"
    , "disconnect"
    , T.unpack netName
    , spec ^. image . to T.unpack
    ]


interpNetworkLS 
  :: MonadIO m
  => MonadCatch m 
  => m Text

interpNetworkLS = readDockerProcess ["network", "ls"]


interpNetworkRM 
  :: MonadIO m 
  => MonadCatch m 
  => [Text] 
  -> m Text

interpNetworkRM nets = liftIO $ do
  liftIO . putStrLn $ 
    "Docker: Removing network " ++ T.unpack (T.intercalate ", " nets)

  readDockerProcess $ ["network", "rm"] ++ fmap T.unpack nets


interpNetworkInspect 
  :: MonadIO m 
  => MonadCatch m 
  => Text 
  -> [Text] 
  -> m Text

interpNetworkInspect format nets = liftIO $ do
  putStrLn $ 
    "Docker: Inspecting network " ++ T.unpack (T.intercalate ", " nets)

  readDockerProcess $ 
    ["network", "inspect", "-f", T.unpack format] 
    ++ fmap T.unpack nets


interpContainerLS 
  :: MonadIO m
  => MonadCatch m 
  => DampfT m Text

interpContainerLS = readDockerProcess $ ["container", "list"]


interpInspect 
  :: MonadIO m 
  => MonadCatch m 
  => Text 
  -> Text 
  -> m Text

interpInspect format id' = liftIO $ do
  putStrLn $ 
    "Docker: Inspecting " ++ T.unpack id'

  readDockerProcess $ 
    ["inspect", "-f", T.unpack format, take 12 $ T.unpack id']
