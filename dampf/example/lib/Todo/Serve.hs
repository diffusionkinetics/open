{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, TemplateHaskell #-}

module Todo.Serve where

import Web.Scotty
import Database.PostgreSQL.Simple.Connect

serve :: IO ()
serve = scotty 3166 $ do
  get "/ping" $ text "pong"