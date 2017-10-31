{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings  #-}

module Beetle.Base where

import Data.Aeson
import qualified Network.Wreq as Wreq
import Lens.Micro ((^.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Database.LevelDB.Higher
import Control.Monad.IO.Class
import System.IO
import Data.Monoid

data Key a = Key { theKey :: String, unKey :: a }

class ToURL a where
  toURL :: a -> String

callAPI :: (ToURL a, FromJSON b, MonadKV m) => a -> m b
callAPI x = do
  let url = toURL x
  mv <- getBS (BS8.pack url)
  let getIt = do
        --liftIO $ hPutStrLn stderr $ "calling api "++url
        rsp <- liftIO (Wreq.get url)
        let jbs = rsp ^. Wreq.responseBody
        --liftIO $ BS.hPutStrLn stderr $ BSL.toStrict $ jbs
        putBS (BS8.pack url) (BSL.toStrict $ jbs)
        case eitherDecode' jbs of
          Right v -> return v
          Left err -> fail $ "decode: "++err
  case mv of
    Just respBS -> do
      --liftIO $ hPutStrLn stderr $ "got cached value"
      case decode $ BSL.fromStrict respBS of
        Nothing -> getIt
        Just v -> return v
    Nothing -> getIt

class MonadIO m => MonadKV m where
  getBS :: BS.ByteString -> m (Maybe BS.ByteString)
  putBS :: BS.ByteString -> BS.ByteString -> m ()
  rmKey :: BS.ByteString -> m ()

instance MonadKV IO where
  getBS _ = return Nothing
  putBS _ _ = return ()
  rmKey _ = return ()

instance MonadKV (LevelDBT IO) where
  getBS k = get k
  putBS k v = put k v
  rmKey k = delete k