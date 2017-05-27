{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, TemplateHaskell #-}

module Dashdo.Serve where

import Dashdo
import Dashdo.Types

import Web.Scotty
import Control.Monad.Trans (liftIO)
import System.Random
import qualified Data.UUID as UUID
import Data.Text.Lazy (fromStrict, Text)
import qualified Data.ByteString.Lazy as BLS

import Data.FileEmbed
import Data.Hashable
import Control.Applicative

getRandomUUID :: IO Text
getRandomUUID = fromStrict . UUID.toText <$> randomIO

dashdoJS :: BLS.ByteString
dashdoJS = BLS.fromStrict $(embedFile "public/js/dashdo.js")

runDashdo :: Dashdo a -> IO ()
runDashdo s = do
  uuid <- getRandomUUID
  -- this is obviously incorrect (if the form fields change dynamically)
  (iniHtml, ff) <- dashdoGenOut s (initial s)
  print $ hash iniHtml
  scotty 3000 $ do
    get "/js/dashdo.js" $ do
      setHeader "Content-Type" "application/javascript"
      raw dashdoJS
    get "/uuid" $ text uuid
    get "/" $ html iniHtml
    post "/" $ do
     pars <- params
     let newval = parseForm (initial s) ff pars
     (thisHtml, _) <- liftIO $ dashdoGenOut s newval
     html $ thisHtml
