{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric #-}

module Youido.Serve where

import Youido.Database
import Web.Spock
import Web.Spock.Config
import Network.Wai.Middleware.RequestLogger (logStdout)

import Database.PostgreSQL.Simple

type Session = ()

serve :: [String] -> IO ()
serve (cfgNm:_) = do
  pool <-  getPool <$> readJSON cfgNm
  spockCfg <- defaultSpockCfg () pool ()
  putStrLn "serving..."
  runSpock 3000 $ spock spockCfg app

app :: SpockM Connection Session () ()
app = do
   middleware $ logStdout
