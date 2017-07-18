module Dampf.Docker
  ( -- * Actions
    buildDocker
  , deployDocker
  ) where

import Control.Lens
import Control.Monad            (void)
import Control.Monad.IO.Class   (MonadIO)

import Dampf.AppFile
import Dampf.Docker.Free
import Dampf.Docker.Types


-- TODO: Rename this buildImages?
buildDocker :: (MonadIO m, HasDampfApp a) => a -> m ()
buildDocker a = runDockerT . iforM_ (a ^. images) $ \n spec ->
    build n (spec ^. dockerFile)


-- TODO: Rename this deployContainers?
deployDocker :: (MonadIO m, HasDampfApp a) => a -> m ()
deployDocker a = runDockerT . iforM_ (a ^. containers) $ \n spec -> do
    stop n
    void (rm n)
    run n spec

