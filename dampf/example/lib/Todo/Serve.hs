{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, TemplateHaskell #-}

module Todo.Serve where

import Web.Scotty

serve :: IO ()
serve = scotty 3166 $ do
  get "/ping" $ text "pong"