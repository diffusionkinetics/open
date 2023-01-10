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

        ex <- domainSslExists name
        if ex
          then writeNginxConfigFile name spec >> reload
          else do
              writeNginxConfigFile name spec { _letsEncrypt = Just False}
              enableNewDomain name spec

              reload

reload :: (MonadIO m) => DampfT m ()
reload = do _ <- liftIO $ system "service nginx reload"
            return ()

enableNewDomain :: (MonadIO m) => Text -> DomainSpec -> DampfT m ()
enableNewDomain name spec = when (fromMaybe False $ _letsEncrypt spec) $ do
    let wwwArg = if fromMaybe False $ _nowww spec 
                    then [] 
                    else ["-d","www."`T.append` name]
    excode <- shelly $ do
                errExit False $ run_ "certbot"
                     (["certonly","-q","--nginx","--expand","-d",name]++wwwArg)
                lastExitCode
    if (excode ==0)
      then writeNginxConfigFile name spec
      else writeNginxConfigFile name spec { _letsEncrypt = Just False}

domainSslExists :: (MonadIO m) => Text -> DampfT m Bool
domainSslExists name = do
    let fnm = ("/etc/nginx/sites-available" </> T.unpack name)
    ex <- liftIO $ doesFileExist fnm
    if not ex
        then return False
        else do fl <- liftIO $ T.readFile fnm
                return $ "ssl_certificate" `T.isInfixOf` fl

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
