{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, TemplateHaskell #-}

module Todo.Serve where

import Todo.Items
import Web.Scotty
import Database.PostgreSQL.Simple.Connect
import Database.PostgreSQL.Simple.Expr
import Database.PostgreSQL.Simple
import Data.String
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8 (unpack)

serve :: IO ()
serve = do
  conn <- createConn =<< configFromEnv
  scotty 3166 $ do
    get "/ping" $ text "pong"
    post "/add" $ do
      bd <- body
      Serial k <- liftIO $ insert conn $ Todo 0 (unpack bd) False
      text $ fromString $ show k
    post "/done/:id" $ do
      tid <- param "id"
      _ <- liftIO $ execute conn "update todos set done = true where id = ?" (Only (tid::Int))
      text "OK"
    get "/list" $ do
      todos <- liftIO $ selectFrom conn "todos" ()
      json (todos::[Todo])
