{-# LANGUAGE OverloadedStrings  #-}

module Dampf.Nginx where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Text                  (Text)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Process
import           System.Exit

import           Dampf.Nginx.Config
import           Dampf.Types
import Shelly hiding ((</>), FilePath)



deployDomains :: (MonadIO m) => DampfT m ()
deployDomains = do
    ds <- view (app . domains)

    iforM_ ds $ \name spec -> do
        moveStaticItems name (spec ^. static)

        ex <- domainExists name
        if ex
          then writeNginxConfigFile name spec >> reload
          else do
              writeNginxConfigFile name spec { _letsEncrypt = Just False}
              enableNewDomain name spec
              writeNginxConfigFile name spec
              reload

reload :: (MonadIO m) => DampfT m ()
reload = do ExitSuccess <- liftIO $ system "service nginx reload"
            return ()

enableNewDomain :: (MonadIO m) => Text -> DomainSpec -> DampfT m ()
enableNewDomain name spec = when (fromMaybe False $ _letsEncrypt spec) $ do
    --certbot-auto certonly -q --webroot --expand -d example.com
    shelly $ run_ "certbot-auto"
      ["certonly","-q","--nginx","--expand","-d",name, "-d","www."`T.append` name]
    return ()


domainExists :: (MonadIO m) => Text -> DampfT m Bool
domainExists name
  = liftIO $ doesFileExist ("/etc/nginx/sites-available" </> T.unpack name)

writeNginxConfigFile :: (MonadIO m) => Text -> DomainSpec -> DampfT m ()
writeNginxConfigFile name spec = do
    fl <- domainConfig name spec

    liftIO $ do
        let strName = T.unpack name

        T.writeFile ("/etc/nginx/sites-available" </> strName) fl
        removePathForcibly ("/etc/nginx/sites-enabled" </> strName)
        createSymbolicLink ("/etc/nginx/sites-available" </> strName)
            ("/etc/nginx/sites-enabled" </> strName)


moveStaticItems :: (MonadIO m) => Text -> Maybe FilePath -> DampfT m ()
moveStaticItems s (Just src) = liftIO $ do
    exists <- doesDirectoryExist dest
    when exists $ removeDirectoryRecursive dest

    createDirectoryIfMissing True dest
    void . system $ "cp -R " ++ (src </> ".") ++ " " ++ dest
  where
    dest = "/var/www" </> T.unpack s

moveStaticItems _ Nothing    = return ()

