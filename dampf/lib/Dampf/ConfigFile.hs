{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Dampf.ConfigFile
  ( -- * Configuration Types
    DampfConfig(..)
  , HasDampfConfig(..)
  , PostgresConfig(..)
  , HasPostgresConfig(..)
    -- * Pretty Printing
  , pShowDampfConfig
    -- * Using Configurations
  , loadConfigFile
  , withConfigFile
  ) where

import           Data.Maybe                 (fromMaybe)
import           Data.Yaml
import           System.Directory           (getHomeDirectory)
import           System.FilePath            ((</>))

import           Dampf.ConfigFile.Pretty
import           Dampf.ConfigFile.Types
import           Dampf.Internal.Env


-- Using Configurations

withConfigFile :: Maybe FilePath -> (DampfConfig -> IO ()) -> IO ()
withConfigFile mf action = loadConfigFile mf >>= action
{-# INLINE withConfigFile #-}


loadConfigFile :: Maybe FilePath -> IO DampfConfig
loadConfigFile mf = do
    homeCfg <- fmap (</> ".dampfcfg.yaml") getHomeDirectory
    parseConfig $ fromMaybe homeCfg mf
{-# INLINE loadConfigFile #-}


parseConfig :: FilePath -> IO DampfConfig
parseConfig f = decodeFile f >>= \case
    Just y  -> resolveEnvVars y >>= parseMonad parseJSON
    Nothing -> error "Could not load config"

