{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric #-}

module Youido.Serve where

import Youido.Database
import Youido.Types
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdout)

import Database.PostgreSQL.Simple
import Control.Monad.IO.Class
import Control.Monad.Reader

type Session = ()

--conn <-  createConn <$> readJSON "youido.json"

serve :: a -> [Handler (ReaderT a IO)] -> IO ()
serve x hs = do

  scotty 3000 $ do
   middleware $ logStdout
   matchAny (regex "/*") $ do
     rq <- request
     liftIO $ print ("got request", rq)
     Response stat hdrs conts <- liftIO $ runReaderT (run hs "Youido - Not Found!" rq) x
     status stat
     mapM_ (uncurry setHeader) hdrs
     raw conts

