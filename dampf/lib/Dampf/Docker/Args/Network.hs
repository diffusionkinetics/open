{-# LANGUAGE TemplateHaskell #-}
module Dampf.Docker.Args.Network where

import Dampf.Docker.Args.Class
import Data.Text (Text)
import Control.Lens
import Data.Monoid ((<>))

import qualified Data.Text as T

data Driver = Bridge 

instance Show Driver where 
  show Bridge = "bridge"

data CreateArgs = CreateArgs {
    _driver :: Driver
  , _createNet :: Text
} 

data ConnectArgs = ConnectArgs {
    _alias :: Maybe Text
  , _ip :: Maybe Text
  , _link :: Maybe [Text]
  , _connectToNet :: Text 
  , _containerName :: Text
}

defCreateArg :: Text -> CreateArgs
defCreateArg = CreateArgs Bridge 

defConnectArg :: Text -> Text -> ConnectArgs
defConnectArg net container = ConnectArgs Nothing Nothing Nothing net container

makeClassy ''ConnectArgs
makeClassy ''CreateArgs

instance ToArgs CreateArgs where
  toArgs s = ["network", "create"]
    <> s ^. driver . to (namedArg "driver")
    <> [s ^. createNet . to T.unpack]


instance ToArgs ConnectArgs where
  toArgs s = ["network", "connect"] 
    <> s ^. alias . _Just . to (namedTextArg "alias")
    <> s ^. ip . _Just . to (namedTextArg "ip")
    <> ["--link"] <> toListOf (link . _Just . traverse . to T.unpack) s
    <> [s ^. connectToNet . to T.unpack]
    <> [s ^. containerName . to T.unpack]
