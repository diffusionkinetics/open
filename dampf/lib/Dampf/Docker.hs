module Dampf.Docker
  ( -- * Actions
    buildDocker
  , deployDocker
  ) where

import Control.Lens
import Control.Monad            (void)
import Control.Monad.Catch      (MonadThrow)
import Control.Monad.IO.Class   (MonadIO)

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

