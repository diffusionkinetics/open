{-# LANGUAGE OverloadedStrings #-}

module Dampf.Postgres.Migrate where

import Control.Arrow              ((&&&))
import Control.Lens
import Control.Monad              (forM_, unless, void, when)
import Control.Monad.Catch        (MonadThrow, throwM)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.Char                  (isDigit)
import Data.List                  (isSuffixOf, sort)
import Data.String                (fromString)
import Data.Text                  (Text)
import Data.Time
import Database.PostgreSQL.Simple
import System.Directory
import System.FilePath

import Dampf.Postgres.Connect
import Dampf.Types


newMigration :: String -> DatabaseSpec -> IO ()
newMigration mig spec = case spec ^. migrations of
    Just m  -> do
        ts <- formatTime defaultTimeLocale "%Y%m%d%H%M%S" <$> getCurrentTime
        let f = m </> ts ++ "_" ++ mig ++ ".sql"

        writeFile f ""
        putStrLn f

    Nothing -> throwM NoMigrations


migrate :: (MonadIO m, MonadThrow m) => Text -> DatabaseSpec -> DampfT m ()
migrate name spec = case spec ^. migrations of
    Just m  -> do
        ms     <- view (config . databaseServer)
        exists <- liftIO $ doesDirectoryExist m

        case ms of
            Just _  -> when exists $ do
                migs <- liftIO $ getMigrations m

                unless (null migs) $ do
                    conn <- createConn name spec
                    done <- liftIO $ getAppliedMigrations conn

                    let migs' = filter ((`notElem` done) . fst) migs

                    liftIO . forM_ migs' $ \(t, f) -> do
                        content <- readFile f
                        putStrLn $ "Applying migration: " ++ t

                        let qStr = content
                                    ++ "; INSERT INTO migrations (timestamp) VALUES ('"
                                    ++ t
                                    ++ "')"

                        execute_ conn $ fromString qStr

            Nothing -> throwM NoDatabaseServer

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

