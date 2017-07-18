{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Dampf.Internal.AppFile.Types
  ( -- * Application Type
    DampfApp(..)
  , HasDampfApp(..)
    -- * Specification Types
  , ImageSpec(..)
  , HasImageSpec(..)
  , ContainerSpec(..)
  , HasContainerSpec(..)
  , DatabaseSpec(..)
  , HasDatabaseSpec(..)
  , DomainSpec(..)
  , HasDomainSpec(..)
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Lazy as HM
import           Data.Map.Strict            (Map)
import           Data.Text                  (Text)
import qualified Data.Text as T
import           GHC.Generics

import           Dampf.Internal.Yaml


data ImageSpec = ImageSpec
    { _dockerFile :: FilePath
    } deriving (Eq, Show, Generic)

makeClassy ''ImageSpec


instance FromJSON ImageSpec where
    parseJSON = gDecode


data ContainerSpec = ContainerSpec
    { _image    :: String
    , _expose   :: Maybe [Int]
    , _command  :: Maybe String
    } deriving (Eq, Show, Generic)

makeClassy ''ContainerSpec


instance FromJSON ContainerSpec where
    parseJSON = gDecode


data DatabaseSpec = DatabaseSpec
    { _migrations   :: Maybe FilePath
    , _user         :: String
    , _extensions   :: [String]
    } deriving (Eq, Show, Generic)

makeClassy ''DatabaseSpec


instance FromJSON DatabaseSpec where
    parseJSON = gDecode


data DomainSpec = DomainSpec
    { _static           :: Maybe FilePath
    , _proxyContainer   :: Maybe Text
    , _letsEncrypt      :: Maybe Bool
    } deriving (Eq, Show, Generic)

makeClassy ''DomainSpec


instance FromJSON DomainSpec where
    parseJSON = gDecode


data DampfApp = DA
    { _images     :: Map Text ImageSpec
    , _containers :: Map Text ContainerSpec
    , _databases  :: Map Text DatabaseSpec
    , _domains    :: Map Text DomainSpec
    } deriving (Eq, Show, Generic)

makeClassy ''DampfApp


instance Monoid DampfApp where
    mempty = DA mempty mempty mempty mempty


    mappend (DA a b c d) (DA a' b' c' d') = DA
        (mappend a a') (mappend b b') (mappend c c') (mappend d d')


instance FromJSON DampfApp where
    parseJSON = withObject "Application File" $ \m ->
        fmap mconcat $ forM (HM.toList m) $ \(k,v) ->
            case T.words k of
                ["image",     name] -> do
                    spec <- parseJSON v
                    return $ (images . at name ?~ spec) mempty

                ["container", name] -> do
                    spec <- parseJSON v
                    return $ (containers . at name ?~ spec) mempty

                ["postgresdb",  name] -> do
                    spec <- parseJSON v
                    return $ (databases . at name ?~ spec) mempty

                ["domain",    name] -> do
                    spec <- parseJSON v
                    return $ (domains . at name ?~ spec) mempty

                _                   -> fail "Invalid specification type"

