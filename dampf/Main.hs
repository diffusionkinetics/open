{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Data.Maybe                     (fromJust)
import           Data.Semigroup
import           Data.Text                      (Text)
import qualified Data.Text as T
import           Options.Applicative            (Parser, ReadM)
import qualified Options.Applicative as O
import qualified Options.Applicative.Types as O

import           Dampf
import           Dampf.Postgres
import           Dampf.Types


-- Running Dampfs


main :: IO ()
main = O.execParser parser >>= run
  where
    parser = O.info
        (O.helper <*> parseOptions)
        (O.header "dampf - Declarative DevOps for Busy Developers")


run :: Options -> IO ()
run (Options af cf p cmd) = do
    a <- loadAppFile af
    c <- loadConfigFile cf >>= \case
        Left cfg -> return cfg
        Right ps -> return (ps ^. profiles . at p . to fromJust)

    case cmd of
        Backup db           -> runDampfT a c (backupDB db)
        Build               -> runDampfT a c goBuild
        Deploy              -> runDampfT a c goDeploy
        Dump                -> runDampfT a c dump
        NewMigration db mig -> runDampfT a c (newMigrationCmd db mig)
        RunMigrations db    -> runDampfT a c (runMigrations db)
        SetupDatabase       -> runDampfT a c setupDB


-- Command Line Options

data Options = Options
    { appFile   :: Maybe FilePath
    , cfgFile   :: Maybe FilePath
    , profile   :: Text
    , command   :: Command
    } deriving (Show)


data Command
    = Backup (Maybe Text)
    | Build
    | Deploy
    | Dump
    | NewMigration Text FilePath
    | RunMigrations (Maybe Text)
    | SetupDatabase
    deriving (Show)


parseOptions :: Parser Options
parseOptions = Options
    <$> optional (O.strOption $
           O.short 'a'
        <> O.long "appFile"
        <> O.metavar "FILE"
        <> O.help "The file which specifies the application")

    <*> optional (O.strOption $
           O.short 'c'
        <> O.long "configFile"
        <> O.metavar "FILE"
        <> O.help "The file which specifies the configuration")

    <*> O.option readerText
           (O.short 'p'
        <> O.long "profile"
        <> O.metavar "PROFILE"
        <> O.help "The configuration profile (default: default)"
        <> O.value "default")

    <*> parseCommand


parseCommand :: Parser Command
parseCommand = O.subparser $
       O.command "backup"
           (O.info
               (O.helper <*> parseBackup)
               (O.progDesc "Backup the specified databases"))

    <> O.command "build"
            (O.info
                (O.helper <*> pure Build)
                (O.progDesc "Build the application"))

    <> O.command "deploy"
            (O.info
                (O.helper <*> pure Deploy)
                (O.progDesc "Deploy the application"))

    <> O.command "dump"
            (O.info
                (O.helper <*> pure Dump)
                (O.progDesc "Show the dampf context"))

    <> O.command "newmigration" 
            (O.info
                (O.helper <*> parseNewMigration)
                (O.progDesc "Create a new database migration"))

    <> O.command "runmigrations"
            (O.info 
                (O.helper <*> parseRunMigrations)
                (O.progDesc "Run unapplied database migrations"))

    <> O.command "setupdb"
            (O.info
                (O.helper <*> pure SetupDatabase)
                (O.progDesc "Setup the databases"))


parseBackup :: Parser Command
parseBackup = Backup
    <$> optional (O.argument readerText (O.metavar "DATABASE"))


parseNewMigration :: Parser Command
parseNewMigration = NewMigration
    <$> O.argument readerText (O.metavar "DATABASE")
    <*> O.strArgument (O.metavar "NAME")


parseRunMigrations :: Parser Command
parseRunMigrations = RunMigrations
    <$> optional (O.argument readerText (O.metavar "DATABASE"))


readerText :: ReadM Text
readerText = T.pack <$> O.readerAsk

