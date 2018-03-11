{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric, TypeApplications #-}

module Database.PostgreSQL.Simple.Connect where

import System.Exit (die)
import GHC.Generics
import GHC.Conc

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Control.Exception
import System.Environment

import Database.PostgreSQL.Simple
import Data.Pool
import Data.Time.Clock

data DatabaseConfig = DatabaseConfig
  { user                   :: String
  , password               :: String
  , host                   :: String
  , port                   :: Integer
  , dbname                 :: String
  , numstripes             :: Int
  , keepOpenTime           :: NominalDiffTime
  , resPerStripe           :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON DatabaseConfig

configFromEnv :: IO DatabaseConfig
configFromEnv = DatabaseConfig
  <$> getEnv "PGUSER"
  <*> getEnv "PGPASSWORD"
  <*> getEnv "PGHOST"
  <*> (read <$> getEnv "PGPORT")
  <*> getEnv "PGDATABASE"
  <*> (maybe 2 read <$> (lookupEnv "PGPOOL_NUM_STRIPES"))
  <*> (maybe (24*60*60) (fromIntegral . read @Int) <$>
       (lookupEnv "PGPOOL_KEEP_OPEN_TIME"))
  <*> (maybe 20 read <$> (lookupEnv "PGPOOL_RES_PER_STRIPES"))


createConn :: DatabaseConfig -> IO Connection
createConn config = do
   catch (createConn' config)
         (\(e::SomeException) -> do putStrLn $ "Failed to connect to the database, retrying in 10s ... ("++show e++")"
                                    threadDelay $ 10 * 1000 * 1000
                                    createConn' config)

dbCfgToConnectInfo :: DatabaseConfig -> ConnectInfo
dbCfgToConnectInfo config = ConnectInfo
    { connectHost     = host     config
    , connectUser     = user     config
    , connectPassword = password config
    , connectDatabase = dbname   config
    , connectPort     = fromInteger $ port config
    }

createConn' :: DatabaseConfig -> IO Connection
createConn' = connect . dbCfgToConnectInfo

createConnPool :: DatabaseConfig -> IO (Pool Connection)
createConnPool cfg = createPool (createConn' cfg) close
  (numstripes cfg) (keepOpenTime cfg) (resPerStripe cfg)

readJSON :: FromJSON a => FilePath -> IO a
readJSON path = do
  configJson <- BSL.readFile path
  case eitherDecode configJson of
    Right config -> return config
    Left err -> do
      die $ "Can't read the config file: " ++ err
