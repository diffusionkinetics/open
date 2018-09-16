{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections,
             DeriveGeneric, ExtendedDefaultRules, FlexibleContexts#-}

module Youido.Authentication where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import qualified Data.IntMap


import           System.Random
import           Text.Read (readMaybe)

import           Web.Cookie (Cookies, SetCookie(..), def)
import           Data.ByteString.Char8 (pack, unpack)
--------------------------------------------------------------------------
--- SERVING
--------------------------------------------------------------------------


newSession :: TVar (Data.IntMap.IntMap a) -> a -> IO SetCookie
newSession tv x = do
  n <- randomRIO (0,99999999999)
  atomically $ modifyTVar' tv (Data.IntMap.insert n x)
  return $ def {setCookieName = "youisess", setCookieValue = (pack $ show n)}
  -- setSimpleCookie "youisess" (pack $ show n)

getSessionCookie :: Cookies -> Maybe Int
getSessionCookie cookies = (readMaybe . unpack) =<< lookup "youisess" cookies

lookupSession :: TVar (Data.IntMap.IntMap a) -> Cookies -> IO (Maybe a)
lookupSession tv cookies = do
  mp <- liftIO $ readTVarIO tv
  return $ do
    n <- getSessionCookie cookies
    Data.IntMap.lookup n mp

deleteSession :: TVar (Data.IntMap.IntMap a) -> Cookies -> IO ()
deleteSession tv cookies = atomically . modifyTVar' tv $ maybe id Data.IntMap.delete (getSessionCookie cookies)
