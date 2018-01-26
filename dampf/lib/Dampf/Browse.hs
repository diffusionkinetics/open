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

browse :: (MonadIO m, MonadThrow m) => URL -> DampfT m ()
browse = browse' . w3m

browse' :: (MonadIO m, MonadThrow m) => Browser -> DampfT m ()
browse' (name, spec) = do
  (argsTweak, container_names, netName) <- fakeHostsArgs
  void . runDockerT $ runWith argsTweak name spec
  cleanUp netName (name : container_names)
