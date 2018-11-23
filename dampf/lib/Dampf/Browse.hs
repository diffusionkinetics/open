{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Dampf.Browse where

import Dampf.Test
import Dampf.Types
import Dampf.Docker.Free
import Dampf.Docker.Types
import Dampf.Docker.Args.Run
  
import Data.Text (Text)
import qualified Data.Map.Strict as Map

import GHC.Generics
import Control.Lens

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadCatch)

type ContainerName = Text
type Args = RunArgs -> RunArgs

data Browser = Browser ContainerName ContainerSpec Args

chrome_vnc :: Browser
chrome_vnc = Browser "dampf-chrome-vnc-server" spec id where
  spec = ContainerSpec
    "siomiz/chrome"
    Nothing
    Nothing
    Nothing

chrome_x11 :: Browser
chrome_x11 = Browser "dampf-chrome-x11" spec args where
  spec = ContainerSpec
    "jess/chrome"
    Nothing
    Nothing
    Nothing
  args = set privileged True
       . set envs (Map.fromList [("DISPLAY","unix:0")])
       . set volumes [("/tmp/.X11-unix/","/tmp/.X11-unix")]

data Backend = VNC | X11 deriving (Show, Read, Eq, Generic)

browse :: (MonadIO m, MonadCatch m) => Backend -> DampfT m ()
browse b =
  let go (Browser name' spec args) = do
        (hosts, argsTweak, container_names, netName) <- fakeHostsArgs
        void . runDockerT $ runWith (args . argsTweak) name' spec
        cleanUp netName (name' : container_names)
     in case b of
          VNC -> go chrome_vnc
          X11 -> go chrome_x11
