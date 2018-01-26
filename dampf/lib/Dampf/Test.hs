{-# language TupleSections, BangPatterns, LambdaCase, OverloadedStrings, ViewPatterns #-}
module Dampf.Test where

import Dampf.Types
import Dampf.Monitor
import Dampf.Docker.Free
import Dampf.Docker.Types
import Dampf.Docker.Args.Run
import Dampf.Nginx.Test

import Data.Text (Text)

import System.Random

import Control.Lens 
import Control.Monad 
import Control.Monad.Reader
import Control.Monad.Catch      (MonadThrow)
import Control.Monad.IO.Class   (MonadIO, liftIO)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

type IP = Text 
type Network = Text
type Volumes = [(FilePath, FilePath)]
type ContainerNames = [Text]

-- add database related opearitons to `test`
-- finalize on exceptions

fakeHostsArgs :: (MonadIO m, MonadThrow m) =>
  DampfT m (RunArgs -> RunArgs, ContainerNames, Network)

fakeHostsArgs = do
  netName <- randomName
  runDockerT $ netCreate netName

  proxie_names <- runProxies netName

  nginx_ip <- pretendToDeployDomains >>= runNginx netName
  liftIO $ print nginx_ip

  fakeHosts <- set mapped nginx_ip <$> view (app . domains)

  let argsTweak = set net netName 
                . set detach (Detach False)
                . set hosts fakeHosts

  return (argsTweak, nginx_container_name : proxie_names, netName)

test :: (MonadIO m, MonadThrow m) => Tests -> DampfT m ()
test ls = do
  (argsTweak, container_names, netName) <- fakeHostsArgs

  test_container_names <- runTests argsTweak ls
  cleanUp netName (container_names ++ test_container_names)

nginx_container_name = "dampf-nginx"

runProxies :: (MonadIO m, MonadThrow m) => Network -> DampfT m ContainerNames
runProxies netName = do
  let pxs = app . domains . traversed . proxyContainer . _Just . to (head . T.splitOn ":")

  proxies <- ask <&> toListOf pxs

  let cts = app . containers . to (Map.filter
        (^. image . to (flip elem proxies)))

  names <- view cts >>= imapM (\n -> 
    runDockerT . fmap (const n) . runWith (set net netName) n)

  return (toListOf traverse names)

runNginx :: (MonadIO m, MonadThrow m) => Network -> Volumes -> DampfT m IP
runNginx netName vs = runDockerT $ 
  runWith xargs nginx_container_name xSpec >>= getIp . T.take 12 
    where 
      getIp (head . T.lines -> !id') = head . T.lines <$> inspect 
        "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}" id'

      xargs = set net netName 
            . set volumes vs 
            . set publish [Port 433, Port 80] 
            {-. set detach (Detach False)-}

      xSpec = ContainerSpec "nginx" Nothing Nothing Nothing

cleanUp :: (MonadIO m, MonadThrow m) => Network -> Names -> DampfT m ()
cleanUp netName names = void . runDockerT $ do
  stopMany names
  void (rmMany names)
  netRM [netName]

randomName :: (MonadIO m, MonadThrow m) => DampfT m Network
randomName = fmap T.pack . replicateM 16 . liftIO . randomRIO $ ('a','z')
