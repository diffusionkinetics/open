{-# LANGUAGE OverloadedStrings #-}

module Dampf.Postgres.Migrate where

import Control.Monad

import Data.List
import Data.String
import Data.Time
import System.Directory
import System.FilePath
import Dampf.ConfigFile
import Dampf.AppFile
import Dampf.Postgres.Connect

import Database.PostgreSQL.Simple

digits :: String
digits = "0123456789"

migrate :: DampfConfig -> String -> DBSpec -> IO ()
migrate cfg dbnm dbspec = do
  let Just migrationsPath = migrations dbspec
  ex <- doesDirectoryExist migrationsPath
  when ex $ do
    fileNames <- getDirectoryContents migrationsPath
    let possibles = sortBy (\(a,_) (b,_) -> compare a b)
                    . filter ((".sql" `isSuffixOf`) . snd)
                    . map (\n -> (takeWhile (`elem` digits) n, migrationsPath </> n))
                    $ fileNames

    when (not $ null possibles) $ do

      conn   <- createConn dbnm dbspec cfg

      alreadyMigratedTimestamps <- getAlreadyMigratedTimestamps conn


      let toMigrate = filter ((`notElem` alreadyMigratedTimestamps) . fst) possibles

      forM_ toMigrate $ \(t,p) -> do
        content <- readFile p
        putStrLn $ "Migrating: "++t
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