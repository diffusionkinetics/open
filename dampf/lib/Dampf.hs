module Dampf where

import Control.Lens
import Control.Monad.Catch      (MonadThrow)
import Control.Monad.IO.Class   (MonadIO, liftIO)

import Dampf.Docker
import Dampf.Nginx
import Dampf.Postgres
import Dampf.Types


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
    deployDocker
    runMigrations Nothing
    deployDomains

