{-# LANGUAGE OverloadedStrings  #-}
module Dampf.Docker
  ( -- * Actions
    buildDocker
  , deployDocker
  , runDocker
  ) where

import Control.Lens
import Control.Monad            (void)
import Control.Monad.Catch      (MonadThrow)
import Control.Monad.IO.Class   (MonadIO)
import Data.Text (Text)
import Data.Monoid
import           Data.Map.Strict            (keys)

import Dampf.Docker.Free
import Dampf.Docker.Types
import Dampf.Types


-- TODO: Rename this buildImages?
buildDocker :: (MonadIO m, MonadThrow m) => DampfT m ()
buildDocker = do
    is <- view (app . images)
    runDockerT . iforM_ is $ \n spec ->
        build n (spec ^. dockerFile)


-- TODO: Rename this deployContainers?
deployDocker :: (MonadIO m, MonadThrow m) => DampfT m ()
deployDocker = do
    cs <- view (app . containers)
    runDockerT . iforM_ cs $ \n spec -> do
        stop n
        void (rm n)
        run n spec

runDocker :: (MonadIO m, MonadThrow m) => Text -> Maybe Text -> DampfT m ()
runDocker imgNm mCmd = do
  dbs <- view (app . databases)
  let firstDb = safeHead $ keys dbs
  runDockerT $ run ("run"<>imgNm) $ ContainerSpec imgNm Nothing mCmd firstDb

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead [] = Nothing