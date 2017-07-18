{-# LANGUAGE OverloadedStrings #-}

module Dampf.Postgres.Migrate where

import Control.Arrow              ((&&&))
import Control.Lens
import Control.Monad              (forM_, unless, void, when)
import Control.Monad.Catch        (MonadThrow)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.Char                  (isDigit)
import Data.List                  (isSuffixOf, sort)
import Data.String                (fromString)
import Data.Text                  (Text)
import Data.Time
import Database.PostgreSQL.Simple
import System.Directory
import System.FilePath

import Dampf.AppFile
import Dampf.ConfigFile
import Dampf.Postgres.Connect
import Dampf.Types


newMigration :: String -> DatabaseSpec -> IO ()
newMigration mig spec = case spec ^. migrations of
    Just m  -> do
        ts <- formatTime defaultTimeLocale "%Y%m%d%H%M%S" <$> getCurrentTime
        let f = m </> ts ++ "_" ++ mig ++ ".sql"

        writeFile f ""
        putStrLn f

    Nothing -> error "Database does not have a migrations directory"


migrate :: (MonadIO m, MonadThrow m) => Text -> DatabaseSpec -> DampfT m ()
migrate name spec = case spec ^. migrations of
    Just m  -> do
        ss     <- view (config . databaseServers)
        exists <- liftIO $ doesDirectoryExist m

        when exists $ do
            ms <- liftIO $ getMigrations m

            unless (null ms) . iforM_ ss $ \server _ -> do
                conn <- createConn server name spec
                done <- liftIO $ getAppliedMigrations conn

                let ms' = filter ((`notElem` done) . fst) ms

                liftIO . forM_ ms' $ \(t, f) -> do
                    content <- readFile f
                    putStrLn $ "Applying migration: " ++ t

                    let qStr = content
                            ++ "; INSERT INTO migrations (timestamp) VALUES ('"
                            ++ t
                            ++ "')"

                    execute_ conn $ fromString qStr


    Nothing -> return ()


getAppliedMigrations :: Connection -> IO [String]
getAppliedMigrations conn = do
    void $ execute_ conn
        "CREATE TABLE IF NOT EXISTS migrations (timestamp varchar(15) PRIMARY KEY)"

    rows <- query_ conn "SELECT timestamp FROM migrations"
    return (fmap head rows)


getMigrations :: FilePath -> IO [(String, FilePath)]
getMigrations d =
    fmap (takeWhile isDigit &&& (d </>)) . sort . filter (isSuffixOf ".sql")
        <$> getDirectoryContents d

