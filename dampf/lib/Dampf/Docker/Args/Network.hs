{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
module Dampf.Docker.Args.Network where

import Dampf.Docker.Args.Class
import Data.Text (Text)
import Control.Lens
import Data.Monoid ((<>))
import qualified Data.Text as T

data Driver = Bridge 

class DefArgs a where
  def :: a

instance Show Driver where 
  show Bridge = "bridge"

data Create = Create {
    _driver :: Driver
} 

instance DefArgs Create where
  def = Create Bridge

-- data Connect = Connect {
--     _alias :: Maybe Arg
--   , _ip :: Maybe Arg
--   , _link :: Maybe Arg
-- } deriving (Foldable)

{-instance ToArgs Connect where toArgs = foldArgs-}

instance DefArgs Connect where
  def = Connect Nothing Nothing Nothing

data Connect = Connect {
    _alias :: Maybe Text
  , _ip :: Maybe Text
  , _link :: Maybe [Text]
}

makeClassy ''Connect
makeClassy ''Create

instance ToArgs Connect where
  toArgs s = ["network", "connect"] 
    <> s ^. alias . _Just . to (namedTextArg "alias")
    <> s ^. ip . _Just . to (namedTextArg "ip")
    <> ["--link"] <> toListOf (link . _Just . traverse . to T.unpack) s
