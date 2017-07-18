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
  ) where

import Data.Maybe                 (fromMaybe)
import System.Directory           (getHomeDirectory)
import System.FilePath            ((</>))

import Dampf.Internal.ConfigFile.Pretty
import Dampf.Internal.ConfigFile.Types
import Dampf.Internal.Yaml


-- Using Configurations

loadConfigFile :: Maybe FilePath -> IO DampfConfig
loadConfigFile mf = do
    homeCfg <- fmap (</> ".dampfcfg.yaml") getHomeDirectory
    parseYaml $ fromMaybe homeCfg mf
{-# INLINE loadConfigFile #-}

