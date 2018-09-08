module Dampf where

import Control.Lens
import Control.Monad.Catch      (MonadThrow)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import GHC.Conc

import Dampf.Docker
import Dampf.Nginx
import Dampf.Postgres
import Dampf.Types
import Dampf.Monitor

dump :: (MonadIO m) => DampfT m ()
dump = do
    a <- view app
    c <- view config

    liftIO $ do
        putStrLn $ pShowDampfApp a
        putStrLn $ pShowDampfConfig c


goBuild :: (MonadIO m, MonadThrow m) => DampfT m ()
goBuild = do
    setupDB
    buildDocker


goDeploy :: (MonadIO m, MonadThrow m) => DampfT m ()
goDeploy = do
    goBuild
    runMigrations Nothing
    deployDocker
    deployDomains
    liftIO $ threadDelay 1000000
    runMonitor []
