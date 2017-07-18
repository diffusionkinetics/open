{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Dampf.AppFile
  ( -- * App File Types
    DampfApp(..)
  , HasDampfApp(..)
  , ImageSpec(..)
  , HasImageSpec(..)
  , ContainerSpec(..)
  , HasContainerSpec(..)
  , DatabaseSpec(..)
  , HasDatabaseSpec(..)
  , DomainSpec(..)
  , HasDomainSpec(..)
    -- * Pretty Printing
  , pShowDampfApp
    -- * Using App Files
  , loadAppFile
  ) where

import           Data.Maybe                     (fromMaybe)

import           Dampf.Internal.AppFile.Pretty
import           Dampf.Internal.AppFile.Types
import           Dampf.Internal.Yaml


-- Using App Files

loadAppFile :: Maybe FilePath -> IO DampfApp
loadAppFile = parseYaml . fromMaybe "dampf.yaml"
{-# INLINE loadAppFile #-}

