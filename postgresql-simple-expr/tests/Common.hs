module Common where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Connect
import Control.Monad.Reader

rr :: Connection -> ReaderT Connection IO a -> IO a
rr conn r = runReaderT r conn

setupPG :: IO Connection
setupPG = do
  cfg <- configFromEnv
  createConn' cfg
