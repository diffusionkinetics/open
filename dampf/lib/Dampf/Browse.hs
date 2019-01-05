{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
module Dampf.Browse where

import Dampf.Test
import Dampf.Types
import Dampf.Docker.Free
import Dampf.Docker.Types
import Dampf.Docker.Args.Run
import Dampf.Nginx (pretendToDeployDomains)
  
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import GHC.Generics (Generic)
import Control.Lens

import Control.Monad (void)
import Control.Monad.Reader (ask)
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
  args = 
    set privileged True
    . set envs [("DISPLAY","unix:0")]
    . set volumes [("/tmp/.X11-unix/","/tmp/.X11-unix")]

data Backend = VNC | X11 deriving (Show, Read, Eq, Generic)

browse :: (MonadCatch m, MonadIO m) => Backend -> DampfT m ()
browse b = do
  netName <- randomName
  proxies <- ask <&> toListOf 
    (app.domains.traversed.proxyContainer._Just.to 
      (head . T.splitOn ":"))
  
  let containerMess = nginx_container_name : browserName : proxies
      onlyProxyContainers = 
        app.containers.to (Map.filter 
          (^. image . to (flip elem proxies)))
      (Browser browserName browserSpec browserArgs) = case b of
        VNC -> chrome_vnc
        X11 -> chrome_x11

  void . runDockerT $ do
      netCreate netName

      view onlyProxyContainers >>= imapM_ (runWith (set net netName))

      nginx_ip <- pretendToDeployDomains >>= runNginx netName

      fakeHosts <- set mapped nginx_ip <$> view (app . domains)
      let runArgsTweak =  set net netName 
                        . set detach (Detach False)
                        . set hosts fakeHosts

      _ <- runWith (browserArgs . runArgsTweak) browserName browserSpec

      stopMany containerMess
      void (rmMany containerMess)
      netRM [netName]
