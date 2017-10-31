{-# LANGUAGE DeriveGeneric, FlexibleInstances  #-}

module Beetle.Base where

import Data.Aeson
import qualified Network.Wreq as Wreq
import Lens.Micro ((^.))
import Data.ByteString
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Database.LevelDB.Higher
import Control.Monad.IO.Class

data Key a = Key { theKey :: String, unKey :: a }

class ToURL a where
  toURL :: a -> String

callAPI :: (ToURL a, FromJSON b, MonadKV m, MonadIO m) => a -> m b
callAPI x = do
  let url = toURL x
  mv <- getBS (BS8.pack url)
  let getIt = do
        rsp <- liftIO (Wreq.get url)
        let jbs = rsp ^. Wreq.responseBody
        putBS (BS8.pack url) (BSL.toStrict $ jbs)
        case decode jbs of
          Just v -> return v
  case mv of
    Just respBS -> do
      case decode $ BSL.fromStrict respBS of
        Nothing -> getIt
        Just v -> return v
    Nothing -> getIt

class Monad m => MonadKV m where
  getBS :: ByteString -> m (Maybe ByteString)
  putBS :: ByteString -> ByteString -> m ()
  rmKey :: ByteString -> m ()

instance MonadKV IO where
  getBS _ = return Nothing
  putBS _ _ = return ()
  rmKey _ = return ()

instance MonadKV (LevelDBT IO) where
  getBS k = get k
  putBS k v = put k v
  rmKey k = delete k