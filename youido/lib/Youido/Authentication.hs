{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections,
             DeriveGeneric, ExtendedDefaultRules, FlexibleContexts#-}

module Youido.Authentication where

import Youido.Types
import Web.Scotty
import Web.Scotty.Cookie
import Control.Concurrent.STM
import qualified Data.IntMap
import Control.Monad.IO.Class
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding
import System.Random
import Text.Read (readMaybe)
import Data.Proxy
import Crypto.BCrypt
import Data.ByteString(ByteString)

hashPassword :: Text-> IO HashPassword
hashPassword t = do
  Just p <- hashPasswordUsingPolicy slowerBcryptHashingPolicy $ encodeUtf8 t
  return $ HashPassword p

validatePass :: HashPassword -> ByteString -> Bool
validatePass (HashPassword bs1) bs2 = validatePassword bs1 bs2
--------------------------------------------------------------------------
--- SERVING
--------------------------------------------------------------------------


newSession :: TVar (Data.IntMap.IntMap a) -> a -> ActionM ()
newSession tv email = do
  n <- liftIO $ randomRIO (0,99999999999)
  liftIO $ atomically $ modifyTVar' tv (Data.IntMap.insert n email)
  setSimpleCookie "youisess" (pack $ show n)
  return ()

lookupSession :: TVar (Data.IntMap.IntMap a) -> ActionM (Maybe (Int, a))
lookupSession tv = do
  mi <- (>>=readMaybe) . fmap unpack <$> getCookie "youisess"
  case mi of
    Nothing -> return Nothing
    Just i -> do
      mp <- liftIO $ readTVarIO tv
      return $ fmap (i,) $ Data.IntMap.lookup i mp



deleteSession :: TVar (Data.IntMap.IntMap a) -> Int -> ActionM ()
deleteSession tv n = do
  liftIO $ atomically $ modifyTVar' tv (Data.IntMap.delete n)
  deleteCookie "youisess"
  return ()

