{-# LANGUAGE OverloadedStrings  #-}

module Dampf.Nginx where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Text                  (Text)
import qualified Data.Text.IO as T
import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Process

import           Dampf.AppFile
import           Dampf.Nginx.Config
import           Dampf.Types


deployDomains :: (MonadIO m) => DampfT m ()
deployDomains = do
    ds <- view (app . domains)
    
    iforM_ ds $ \name spec -> do
        moveStaticItems name (spec ^. static)
        fl <- domainConfig name spec

        liftIO $ do
            T.writeFile ("/etc/nginx/sites-available" </> show name) fl

            removePathForcibly ("/etc/nginx/sites-enabled" </> show name)
            createSymbolicLink ("/etc/nginx-sites-available" </> show name)
                ("/etc/nginx/sites-enabled" </> show name)

            void $ system "service nginx reload"


moveStaticItems :: (MonadIO m) => Text -> Maybe FilePath -> DampfT m ()
moveStaticItems s (Just src) = liftIO $ do
    exists <- doesDirectoryExist dest
    when exists $ removeDirectoryRecursive dest

    createDirectoryIfMissing True dest
    void . system $ "cp -R " ++ (src </> ".") ++ " " ++ dest
  where
    dest = "/var/www" </> show s

moveStaticItems _ Nothing    = return ()

