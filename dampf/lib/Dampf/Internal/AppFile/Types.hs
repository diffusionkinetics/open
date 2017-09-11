{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DuplicateRecordFields   #-}
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
  , TestSpec(..)
  , HasTestSpec(..)
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Lazy as HM
import           Data.Map.Strict            (Map)
import           Data.Text                  (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Data.Char (toLower)

import           Dampf.Internal.Yaml


data ImageSpec = ImageSpec
    { _dockerFile :: FilePath
    } deriving (Eq, Show, Generic)

makeClassy ''ImageSpec


instance FromJSON ImageSpec where
    parseJSON = gDecode


data ContainerSpec = ContainerSpec
    { _image        :: Text
    , _expose       :: Maybe [Int]
    , _command      :: Maybe Text
    , _useDatabase  :: Maybe Text
    } deriving (Eq, Show, Generic)

makeClassy ''ContainerSpec

instance FromJSON ContainerSpec where
    parseJSON = gDecode

data TestWhen = Deploy | Hourly | Daily | Frequently deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON TestWhen

data TestSpec = TestSpec
  { _tsImage        :: Text
  , _tsCommand      :: Maybe Text
  , _tsWhen         :: [TestWhen]
  } deriving (Eq, Show, Generic)

makeClassy ''TestSpec

instance FromJSON TestSpec where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 3 . map toLower}

data DatabaseSpec = DatabaseSpec
    { _migrations   :: Maybe FilePath
    , _user         :: Text
    , _extensions   :: [Text]
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
    , _tests      :: Map Text TestSpec
    } deriving (Eq, Show, Generic)

makeClassy ''DampfApp


instance Monoid DampfApp where
    mempty = DA mempty mempty mempty mempty mempty


    mappend (DA a b c d e) (DA a' b' c' d' e') = DA
        (mappend a a') (mappend b b') (mappend c c') (mappend d d') (mappend e e')


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

                ["test",    name] -> do
                    spec <- parseJSON v
                    return $ (tests . at name ?~ spec) mempty

                _                   -> fail "Invalid specification type"

