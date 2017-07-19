{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Dampf.Internal.ConfigFile.Types
  ( -- * Configuration Types
    DampfConfig(..)
  , HasDampfConfig(..)
  , PostgresConfig(..)
  , HasPostgresConfig(..)
  ) where

import           Control.Lens
import           Control.Monad              (forM)
import           Data.Aeson
import qualified Data.HashMap.Lazy as HM
import           Data.Map.Strict            (Map)
import           Data.Text                  (Text)
import qualified Data.Text as T
import           GHC.Generics

import           Dampf.Internal.Yaml


-- Configuration Types

data PostgresConfig = PostgresConfig
    { _host  :: String
    , _port  :: Int
    , _users :: Map Text Text
    } deriving (Eq, Show, Generic)

makeClassy ''PostgresConfig


instance FromJSON PostgresConfig where
    parseJSON = gDecode


data DampfConfig = DC
    { _liveCertificate  :: Maybe FilePath
    , _databaseServers  :: Map Text PostgresConfig
    } deriving (Eq, Show, Generic)

makeClassy ''DampfConfig


instance Monoid DampfConfig where
    mempty = DC mempty mempty


    mappend (DC l d) (DC l' d') = DC (mappend l l') (mappend d d')


instance FromJSON DampfConfig where
    parseJSON = withObject "Configuration File" $ \m ->
        fmap mconcat $ forM (HM.toList m) $ \(k, v) ->
            case T.words k of
                ["liveCertificate"] -> do
                    path <- parseJSON v
                    return $ (liveCertificate .~ path) mempty

                ["postgres", name]  -> do
                    spec <- parseJSON v
                    return $ (databaseServers . at name ?~ spec) mempty

                _                   -> fail "Invalid configuration type"

