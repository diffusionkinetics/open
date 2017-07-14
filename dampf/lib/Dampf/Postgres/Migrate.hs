{-# LANGUAGE OverloadedStrings #-}

module Dampf.Postgres.Migrate where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe   (isJust, fromJust)
import Data.String
import Data.Time
import Database.PostgreSQL.Simple
import System.Directory
import System.FilePath

import Dampf.AppFile
import Dampf.ConfigFile
import Dampf.Postgres.Connect


digits :: String
digits = "0123456789"


getMigrations :: FilePath -> IO [(String, FilePath)]
getMigrations dir = do
    files <- getDirectoryContents dir
    return
        . fmap (takeWhile isDigit &&& (dir </>))
        . sort
        $ filter (isSuffixOf ".sql") files


migrate :: (HasDampfConfig c) => String -> DBSpec -> c -> IO ()
migrate db dbSpec cfg
    | isJust mp = do
        exists <- doesDirectoryExist $ fromJust mp

        when exists $ do
            ms <- getMigrations (fromJust mp)

            unless (null ms) $ do
                conn <- createConn db dbSpec cfg
                done <- getAlreadyMigratedTimestamps conn
                let ms' = filter ((`notElem` done) . fst) ms

                forM_ ms' $ \(t, p) -> do
                    content <- readFile p
                    putStrLn $ "Migrating: " ++ t

                    let qStr = content ++ "; INSERT INTO migrations (timestamp) VALUES ('" ++ t ++ "')"

                    execute_ conn $ fromString qStr

    | otherwise = return ()
  where
    mp = migrations dbSpec


getAlreadyMigratedTimestamps :: Connection -> IO [String]
getAlreadyMigratedTimestamps conn = do
    _    <- execute_ conn "CREATE TABLE IF NOT EXISTS migrations ( timestamp varchar(15) PRIMARY KEY)"
    rows <- query_ conn "SELECT timestamp FROM migrations"

    return $ head <$> rows


newMigration :: String -> DBSpec -> IO ()
newMigration mig dbSpec = do
    now <- getCurrentTime
    let ts = formatTime defaultTimeLocale "%Y%m%d%H%M%S" now
    let fp = path </> ts ++ "_" ++ mig ++ ".sql"
    writeFile fp ""
    putStrLn fp
  where
    path = fromJust $ migrations dbSpec

