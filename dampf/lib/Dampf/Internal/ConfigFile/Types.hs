{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Dampf.Internal.ConfigFile.Types
  ( -- * Configuration Types
    DampfProfiles(..)
  , HasDampfProfiles(..)
  , DampfConfig(..)
  , HasDampfConfig(..)
  , PostgresConfig(..)
  , HasPostgresConfig(..)
  ) where

import           Control.Lens
import           Control.Monad              (forM)
import           Data.Aeson
import qualified Data.HashMap.Lazy as HM
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict as Map
import           Data.Text                  (Text)
import qualified Data.Text as T
import           GHC.Generics

import           Dampf.Internal.Yaml


-- Configuration Types

data PostgresConfig = PostgresConfig
    { _host  :: Text
    , _port  :: Int
    , _users :: Map Text Text
    } deriving (Eq, Show, Generic)

makeClassy ''PostgresConfig


instance FromJSON PostgresConfig where
    parseJSON = gDecode


data DampfConfig = DC
    { _liveCertificate  :: Maybe FilePath
    , _postgres         :: Maybe PostgresConfig
    } deriving (Eq, Show, Generic)

makeClassy ''DampfConfig


instance FromJSON DampfConfig where
    parseJSON = gDecode


data DampfProfiles = DP
    { _profiles :: Map Text DampfConfig
    } deriving (Eq, Show, Generic)

makeClassy ''DampfProfiles


instance FromJSON DampfProfiles where
    parseJSON = withObject "Configuration File" $ \o ->
        fmap (DP . Map.fromList) $ forM (HM.toList o) $ \(k, v) ->
            case T.words k of
                ["profile", name] -> (,) <$> return name <*> parseJSON v
                _                 -> fail "Invalid profile specification"

