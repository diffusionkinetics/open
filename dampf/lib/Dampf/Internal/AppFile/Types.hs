{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Dampf.Internal.AppFile.Types
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
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Lazy as HM
import           Data.Text                  (Text)
import qualified Data.Text as T
import           GHC.Generics

import           Dampf.Internal.Yaml


data ImageSpec = ImageSpec
    { _dockerFile :: FilePath
    } deriving (Show, Generic)

makeClassy ''ImageSpec


instance ToJSON ImageSpec where
    toJSON = gEncode


instance FromJSON ImageSpec where
    parseJSON = gDecode


data ContainerSpec = ContainerSpec
    { _image     :: String
    , _expose    :: Maybe [Int]
    , _command   :: Maybe String
    } deriving (Show, Generic)

makeClassy ''ContainerSpec


instance ToJSON ContainerSpec where
    toJSON = gEncode


instance FromJSON ContainerSpec where
    parseJSON = gDecode


data DomainSpec = DomainSpec
    { _static            :: Maybe FilePath
    , _proxyContainer    :: Maybe Text
    , _letsencrypt       :: Maybe Bool
    } deriving (Show, Generic)

makeClassy ''DomainSpec


instance ToJSON DomainSpec where
    toJSON = gEncode


instance FromJSON DomainSpec where
    parseJSON = gDecode


data DBSpec = DBSpec
    { _migrations    :: Maybe FilePath
    , _dbUser        :: String
    , _dbExtensions  :: [String]
    } deriving (Show, Generic)

makeClassy ''DBSpec


instance ToJSON DBSpec where
    toJSON = gEncode


instance FromJSON DBSpec where
    parseJSON = gDecode


data Dampf
    = Image String ImageSpec
    | Domain String DomainSpec
    | PostgresDB String DBSpec
    | Container String ContainerSpec
    deriving (Show, Generic)

makeClassyPrisms ''Dampf


newtype Dampfs = Dampfs
    { _specs :: [Dampf]
    } deriving (Show, Generic)

makeClassy ''Dampfs


instance FromJSON Dampfs where
  parseJSON = withObject "Dampf config" $ \hm ->
    fmap Dampfs $ forM (HM.toList hm) $ \(k,v) ->
      case T.words k of
        ["image", imname] -> Image
            <$> return (T.unpack imname)
            <*> parseJSON v

        ["container", cname] -> Container
            <$> return (T.unpack cname)
            <*> parseJSON v

        ["postgresdb", dbname] -> PostgresDB
            <$> return (T.unpack dbname)
            <*> parseJSON v

        ["domain", dname] -> Domain
            <$> return (T.unpack dname)
            <*> parseJSON v

        _ -> fail $ "unknown dampf spec: "++ T.unpack k

