{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Dampf.AppFile.Types
  ( -- * App File Types
    Dampf(..)
  , Dampfs(..)
  , ImageSpec(..)
  , ContainerSpec(..)
  , DomainSpec(..)
  , DBSpec(..)
  ) where

import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Lazy as HM
import           Data.Text                  (Text)
import qualified Data.Text as T
import           GHC.Generics


data Dampf
    = Image String ImageSpec
    | Domain String DomainSpec
    | PostgresDB String DBSpec
    | Container String ContainerSpec
    deriving (Show)


newtype Dampfs = Dampfs { unDampfs :: [Dampf] }
    deriving (Show)


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


data ImageSpec = ImageSpec { dockerFile :: FilePath }
    deriving (Show, Generic)


instance FromJSON ImageSpec


data ContainerSpec = ContainerSpec
    { image     :: String
    , expose    :: Maybe [Int]
    , command   :: Maybe String
    } deriving (Show, Generic)


instance FromJSON ContainerSpec


data DomainSpec = DomainSpec
    { static            :: Maybe FilePath
    , proxyContainer    :: Maybe Text
    , letsencrypt       :: Maybe Bool
    } deriving (Show, Generic)


instance FromJSON DomainSpec


data DBSpec = DBSpec
    { migrations    :: Maybe FilePath
    , dbUser        :: String
    , dbExtensions  :: [String]
    } deriving (Show, Generic)


instance FromJSON DBSpec

