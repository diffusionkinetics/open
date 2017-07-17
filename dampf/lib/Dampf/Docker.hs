module Dampf.Docker
  ( -- * Actions
    buildDocker
  , deployDocker
    -- * Docker Monad
  , DockerT
    -- * Docker DSL
  , build
  , rm
  , run
  , stop
  ) where

import Control.Monad            (forM_, void)
import Control.Monad.IO.Class   (MonadIO)

import Dampf.AppFile
import Dampf.Docker.Free
import Dampf.Docker.Types


-- TODO: Rename this buildImages?
buildDocker :: (MonadIO m) => Dampfs -> m ()
buildDocker (Dampfs d) = runDockerT $ forM_ is $ \(n, iSpec) ->
    build n (dockerFile iSpec)
  where
    is = [(n, iSpec) | Image n iSpec <- d]


-- TODO: Rename this deployContainers?
deployDocker :: (MonadIO m) => Dampfs -> m ()
deployDocker (Dampfs d) = runDockerT $ forM_ cs $ \(n, cSpec) -> do
    stop n
    void $ rm n
    run n cSpec
  where
    cs = [(n, cSpec) | Container n cSpec <- d]

