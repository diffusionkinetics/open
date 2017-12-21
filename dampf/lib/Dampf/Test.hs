{-# language TupleSections, BangPatterns, LambdaCase, OverloadedStrings, ViewPatterns #-}
module Dampf.Test where

import Dampf.Types
import Dampf.Monitor
import Dampf.Docker.Free
import Dampf.Docker.Types
import Dampf.Docker.Args.Run
import Dampf.Docker.Args.Class
import Dampf.Nginx.Test

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Monoid ((<>))

import System.Random

import Control.Lens 
import Control.Monad 
import Control.Monad.Reader
import Control.Monad.Catch      (MonadThrow)
import Control.Monad.IO.Class   (MonadIO, liftIO)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

type IP = Text 
type ID = Text
type Network = Text
type Volumes = [(FilePath, FilePath)]

test :: (MonadIO m, MonadThrow m) => Tests -> DampfT m ()
test ls = do
  netName <- randomName
  runDockerT $ netCreate netName

  proxie_ids <- runProxies netName

  -- fix nginx config
  -- rename containers
  -- make cleanUp delete all the containers
  -- add database related opearitons to `test`

  (nginx_id, nginx_ip) <- pretendToDeployDomains >>= runNginx netName

  liftIO . putStrLn . T.unpack $ nginx_ip

  let argsTweak = set dns (Just nginx_ip)
                . set net netName 

  test_ids <- runTests argsTweak ls
  cleanUp netName (nginx_id : (proxie_ids ++ test_ids))

nginx_image_name = "dampf-nginx"

xSpec :: ContainerSpec
xSpec = ContainerSpec "nginx" Nothing Nothing Nothing

runProxies :: (MonadIO m, MonadThrow m) => Network -> DampfT m [ID]
runProxies netName = do
  let cut = fmap (T.take 12)
      pxs = app . domains . traversed . proxyContainer . _Just . to (head . T.splitOn ":")

  proxies <- ask <&> toListOf pxs

  let cts = app . containers . to (Map.filter
        (^. image . to (flip elem proxies)))

  id's <- view cts >>= imapM (\n -> 
    runDockerT . cut . runWith (set net netName) n)

  return (toListOf traverse id's)

runNginx :: (MonadIO m, MonadThrow m) => Network -> Volumes -> DampfT m (ID, IP)
runNginx netName vs = runDockerT $ 
  runWith (set net netName . set volumes vs) nginx_image_name xSpec >>= 
    (\id' -> (T.take 12 id',) <$> getIp id')
    where getIp !id' = inspect "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}" id'

cleanUp :: (MonadIO m, MonadThrow m) => Network -> Names -> DampfT m ()
cleanUp netName names = void . runDockerT $ do
  stopMany names
  {-rmMany names-}
  netRM [netName]

randomName :: (MonadIO m, MonadThrow m) => DampfT m Network
randomName = fmap T.pack . replicateM 16 . liftIO . randomRIO $ ('a','z')
