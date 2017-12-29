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
import           Options.Generic

import           Dampf
import           Dampf.Postgres
import           Dampf.Types
import           Dampf.Monitor
import           Dampf.Test
import           Dampf.Docker
import           Dampf.Provision


-- Running Dampfs


main :: IO ()
main = O.execParser parser >>= run
  where
    parser = O.info
        (O.helper <*> parseOptions)
        (O.header "dampf - Declarative DevOps for Busy Developers")


run :: Options -> IO ()
run (Options af cf p (Provision pt)) =
    goProvision pt
run (Options af cf p cmd) = do
    a   <- loadAppFile af
    c   <- loadConfigFile cf >>= \case
        Left ps   -> return (ps ^. profiles . at p . to fromJust)
        Right cfg -> return cfg

    runDampfT a c $ case cmd of
        Backup db           -> backupDB db
        Restore fp mdb      -> restoreDB fp mdb
        Build               -> goBuild
        Deploy              -> goDeploy
        Dump                -> dump
        Run img mcmd        -> runDocker img mcmd
        NewMigration db mig -> newMigrationCmd db mig
        RunMigrations db    -> runMigrations db
        SetupDatabase       -> setupDB
        Monitor tests       -> runMonitor tests
        Test tests          -> test tests
        Provision pt        -> goProvision pt
        Env cmds            -> envCmd cmds

-- Command Line Options

data Options = Options
    { appFile   :: Maybe FilePath
    , cfgFile   :: Maybe FilePath
    , profile   :: Text
    , command   :: Command
    } deriving (Show)


data Command
    = Backup (Maybe Text)
    | Restore FilePath (Maybe Text)
    | Build
    | Deploy
    | Run Text (Maybe Text)
    | Dump
    | NewMigration Text FilePath
    | RunMigrations (Maybe Text)
    | Env [Text]
    | SetupDatabase
    | Monitor [Text]
    | Test [Text]
    | Provision ProvisionType
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
    <> O.command "restore"
            (O.info
                (O.helper <*> parseRestore)
                (O.progDesc "Restore database"))

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
    <> O.command "run"
            (O.info
                (O.helper <*> parseRun)
                (O.progDesc "Run container"))
    <> O.command "setupdb"
            (O.info
                (O.helper <*> pure SetupDatabase)
                (O.progDesc "Setup the databases"))

    <> O.command "monitor"
            (O.info
                (O.helper <*> parseMonitor)
                (O.progDesc "Run specified test (if not specified, run all tests) against live production environment"))
    <> O.command "env"
            (O.info
                (O.helper <*> parseEnv)
                (O.progDesc "Run command locally in an environment that can access database"))
    <> O.command "provision"
            (O.info
                (O.helper <*> parseProvision)
                (O.progDesc "Provision a server"))

    <> O.command "test"
            (O.info
                (O.helper <*> parseTest)
                (O.progDesc "Run specified test (if not specified, all tests)"))

instance ParseField ProvisionType

parseProvision :: Parser Command
parseProvision = Provision
    <$> parseField (Just "SingleServer | Development | CI") Nothing

parseBackup :: Parser Command
parseBackup = Backup
    <$> optional (O.argument readerText (O.metavar "DATABASE"))

parseRestore :: Parser Command
parseRestore = Restore
        <$> O.argument O.readerAsk (O.metavar "FILE")
        <*> optional (O.argument readerText (O.metavar "DATABASE"))


parseRun :: Parser Command
parseRun = Run
    <$> O.argument readerText (O.metavar "IMAGE")
    <*> optional (O.argument readerText (O.metavar "COMMAND"))

parseNewMigration :: Parser Command
parseNewMigration = NewMigration
    <$> O.argument readerText (O.metavar "DATABASE")
    <*> O.strArgument (O.metavar "NAME")


parseRunMigrations :: Parser Command
parseRunMigrations = RunMigrations
    <$> optional (O.argument readerText (O.metavar "DATABASE"))


parseTest :: Parser Command
parseTest = Test
    <$> many (O.argument readerText (O.metavar "TESTS"))

parseMonitor :: Parser Command
parseMonitor = Monitor
    <$> many (O.argument readerText (O.metavar "TESTS"))


parseEnv :: Parser Command
parseEnv = Env
    <$> many (O.argument readerText (O.metavar "CMDS"))

readerText :: ReadM Text
readerText = T.pack <$> O.readerAsk

