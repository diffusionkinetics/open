{-# LANGUAGE OverloadedStrings #-}

module Dampf.Postgres.Migrate where

import Control.Monad

import Data.List
import Data.String
import Data.Time
import System.Directory
import System.FilePath
import Dampf.AppFile
import Dampf.Postgres.Connect

import Database.PostgreSQL.Simple

digits :: String
digits = "0123456789"

migrate :: String -> DBSpec -> IO ()
migrate dbnm dbspec = do
  let Just migrationsPath = migrations dbspec
  conn   <- createConn dbnm dbspec

  alreadyMigratedTimestamps <- getAlreadyMigratedTimestamps conn

  fileNames <- getDirectoryContents migrationsPath

  let toMigrate = sortBy (\(a,_) (b,_) -> compare a b)
                . filter ((".sql" `isSuffixOf`) . snd)
                . filter ((`notElem` alreadyMigratedTimestamps) . fst)
                . map (\n -> (takeWhile (`elem` digits) n, migrationsPath </> n))
                $ fileNames

  forM_ toMigrate $ \(t,p) -> do
    content <- readFile p
    let qStr = content ++ "; INSERT INTO migrations ( timestamp ) VALUES ('" ++ t ++ "')"
        q    = fromString qStr
    execute_ conn q

getAlreadyMigratedTimestamps :: Connection -> IO [String]
getAlreadyMigratedTimestamps conn = do
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS migrations ( timestamp varchar(15) PRIMARY KEY)"

  rows <- query_ conn "SELECT timestamp FROM migrations"
  return $ map head rows

newMigration :: String -> DBSpec -> IO ()
newMigration mignm dbspec = do
  let Just migrationsPath = migrations dbspec
  now <- getCurrentTime
  let ts = formatTime defaultTimeLocale "%Y%m%d%H%M%S" now
  let fp = migrationsPath </> ts ++ "_" ++ mignm ++ ".sql"
  writeFile fp ""
  putStrLn fp