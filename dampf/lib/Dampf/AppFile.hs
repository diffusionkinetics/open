{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Dampf.AppFile
  ( -- * App File Types
    Dampf(..)
  , AsDampf(..)
  , Dampfs(..)
  , HasDampfs(..)
  , ImageSpec(..)
  , HasImageSpec(..)
  , ContainerSpec(..)
  , HasContainerSpec(..)
  , DomainSpec(..)
  , HasDomainSpec(..)
  , DBSpec(..)
  , HasDBSpec(..)
    -- * Pretty Printing
  , pShowDampfs
    -- * Using App Files
  , loadAppFile
  , withAppFile
  ) where

import           Data.Maybe                     (fromMaybe)

import           Dampf.Internal.AppFile.Pretty
import           Dampf.Internal.AppFile.Types
import           Dampf.Internal.Yaml


-- Using App Files

withAppFile :: Maybe FilePath -> (Dampfs -> IO ()) -> IO ()
withAppFile mf action = loadAppFile mf >>= action
{-# INLINE withAppFile #-}


loadAppFile :: Maybe FilePath -> IO Dampfs
loadAppFile = parseYaml . fromMaybe "dampf.yaml"
{-# INLINE loadAppFile #-}

