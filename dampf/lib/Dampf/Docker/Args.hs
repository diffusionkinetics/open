module Dampf.Docker.Args
  ( ToArgs(..)
  , RunArgs
  , HasRunArgs(..)
  , mkRunArgs
  , defCreateArg
  , defConnectArg
  , ConnectArgs
  , CreateArgs
  , unDaemonize
  ) where

import           Dampf.Docker.Args.Class
import           Dampf.Docker.Args.Run
import           Dampf.Docker.Args.Network
