{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Dampf.AppFile
  ( -- * App File Types
    Dampf(..)
  , Dampfs(..)
  , ImageSpec(..)
  , ContainerSpec(..)
  , DomainSpec(..)
  , DBSpec(..)
    -- * Pretty Printing
  , pShowDampfs
    -- * Using App Files
  , loadAppFile
  , withAppFile
  ) where

import           Data.Maybe                 (fromMaybe)
import           Data.Yaml

import           Dampf.Internal.AppFile.Pretty
import           Dampf.Internal.AppFile.Types
import           Dampf.Internal.Env


-- Using App Files

withAppFile :: Maybe FilePath -> (Dampfs -> IO ()) -> IO ()
withAppFile mf action = loadAppFile mf >>= action
{-# INLINE withAppFile #-}


loadAppFile :: Maybe FilePath -> IO Dampfs
loadAppFile = parseAppFile . fromMaybe "dampf.yaml"
{-# INLINE loadAppFile #-}


parseAppFile :: FilePath -> IO Dampfs
parseAppFile f = decodeFile f >>= \case
    Just y  -> resolveEnvVars y >>= parseMonad parseJSON
    Nothing -> error "Could not load app file"

