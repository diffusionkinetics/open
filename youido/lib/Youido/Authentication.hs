{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections,
             DeriveGeneric, ExtendedDefaultRules, FlexibleContexts#-}

module Youido.Authentication where

import Web.Scotty
import Web.Scotty.Cookie
import Control.Concurrent.STM
import qualified Data.IntMap
import Control.Monad.IO.Class
import Data.Text (Text, unpack, pack)

import System.Random
import Text.Read (readMaybe)

import Data.ByteString(ByteString)

--------------------------------------------------------------------------
--- SERVING
--------------------------------------------------------------------------


newSession :: TVar (Data.IntMap.IntMap a) -> a -> ActionM ()
newSession tv x = do
  n <- liftIO $ randomRIO (0,99999999999)
  liftIO $ atomically $ modifyTVar' tv (Data.IntMap.insert n x)
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

