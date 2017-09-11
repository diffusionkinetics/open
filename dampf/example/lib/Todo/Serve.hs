{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, TemplateHaskell #-}

module Todo.Serve where

import Web.Scotty

main :: IO ()
main = scotty 3166 $ do
  get "/ping" $ text "pong"