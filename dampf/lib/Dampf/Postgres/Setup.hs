{-# LANGUAGE OverloadedStrings #-}

module Dampf.Postgres.Setup where

import Control.Monad

import Data.List
import Data.String
import Data.Time
import System.Directory
import System.FilePath
import Dampf.AppFile
import Dampf.ConfigFile
import Dampf.Postgres.Connect

import Database.PostgreSQL.Simple

createUsers :: Dampfs -> DampfConfig -> IO ()
createUsers app cfg = return ()

createExtensions :: Dampfs -> DampfConfig -> IO ()
createExtensions app cfg = return ()