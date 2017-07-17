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
import           Data.Aeson.Types
import qualified Data.HashMap.Lazy as HM
import           Data.Text                  (Text)
import qualified Data.Text as T
import           GHC.Generics


data ImageSpec = ImageSpec
    { _dockerFile :: FilePath
    } deriving (Show, Generic)

makeClassy ''ImageSpec


instance FromJSON ImageSpec where
    parseJSON = genericParseJSON opts
      where
        opts = defaultOptions { fieldLabelModifier = drop 1 }


data ContainerSpec = ContainerSpec
    { _image     :: String
    , _expose    :: Maybe [Int]
    , _command   :: Maybe String
    } deriving (Show, Generic)

makeClassy ''ContainerSpec


instance FromJSON ContainerSpec where
    parseJSON = genericParseJSON opts
      where
        opts = defaultOptions { fieldLabelModifier = drop 1 }


data DomainSpec = DomainSpec
    { _static            :: Maybe FilePath
    , _proxyContainer    :: Maybe Text
    , _letsencrypt       :: Maybe Bool
    } deriving (Show, Generic)

makeClassy ''DomainSpec


instance FromJSON DomainSpec where
    parseJSON = genericParseJSON opts
      where
        opts = defaultOptions { fieldLabelModifier = drop 1 }


data DBSpec = DBSpec
    { _migrations    :: Maybe FilePath
    , _dbUser        :: String
    , _dbExtensions  :: [String]
    } deriving (Show, Generic)

makeClassy ''DBSpec


instance FromJSON DBSpec where
    parseJSON = genericParseJSON opts
      where
        opts = defaultOptions { fieldLabelModifier = drop 1 }


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

