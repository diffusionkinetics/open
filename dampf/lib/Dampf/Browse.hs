{-# LANGUAGE OverloadedStrings #-}
module Dampf.Browse where

import Dampf.Test
import Dampf.Types
import Dampf.Docker.Free
import Dampf.Docker.Types
  
import Data.Text (Text)

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)

type Browser = (Text, ContainerSpec)
type URL = Text

w3m :: URL -> Browser
w3m url = ("dampf-w3m", spec url) where
  spec url' = ContainerSpec
    "corbinu/alpine-w3m"
    (Just [ Port 80 ])
    (Just url')
    Nothing

chrome_vnc :: Browser
chrome_vnc = ("dampf-chrome-vnc-server", spec) where
  spec = ContainerSpec
    "siomiz/chrome"
    Nothing
    Nothing
    Nothing

runVNC :: (MonadIO m, MonadThrow m) => DampfT m ()
runVNC = do
  (argsTweak, container_names, netName) <- fakeHostsArgs
  void . runDockerT $ uncurry (runWith argsTweak) chrome_vnc
  cleanUp netName (fst chrome_vnc : container_names)

-- browse :: (MonadIO m, MonadThrow m) => URL -> DampfT m ()
-- browse = browse' . w3m

browse _ = runVNC

browse' :: (MonadIO m, MonadThrow m) => Browser -> DampfT m ()
browse' (name, spec) = do
  (argsTweak, container_names, netName) <- fakeHostsArgs
  void . runDockerT $ runWith argsTweak name spec
  cleanUp netName (name : container_names)
