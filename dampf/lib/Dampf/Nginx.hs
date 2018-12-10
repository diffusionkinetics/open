{-# LANGUAGE OverloadedStrings  #-}
{-# language FlexibleContexts #-}

module Dampf.Nginx where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Text                  (Text)
import           Data.Monoid ((<>))
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Process
import           System.Exit

import           Dampf.Nginx.Config
import           Dampf.Nginx.Types (IsTest)
import           Dampf.Types
import Shelly hiding ((</>), FilePath)



deployDomains   
  :: (MonadReader DampfContext m, MonadIO m) => m ()
deployDomains = do
    ds <- view (app . domains)

    iforM_ ds $ \name spec -> do
        moveStaticItems name (spec ^. static)

        ex <- domainSslExists name
        if ex
          then writeNginxConfigFile False name spec >> reload
          else do
               writeNginxConfigFile False name spec { _letsEncrypt = Just False}
               enableNewDomain False name spec

               reload


pretendToDeployDomains 
  :: (MonadReader DampfContext m, MonadIO m) 
  => m [(FilePath, FilePath)]

pretendToDeployDomains = do
    ds  <- view $ app . domains

    let go = (\n -> (n, "/var/www" <> n))
        path = "/tmp/dampf/test-nginx"
    
    fl <- itraverse (domainConfig True) ds <&> foldOf traverse
      
    -- for testing purpouses only 
    -- liftIO $ do
    --   putStrLn " -- nginx config file -- "
    --   T.putStrLn fl 
    --   putStrLn " -- end of config -- "
      

    liftIO $ do 
      createDirectoryIfMissing True path
      T.writeFile (path </> "nginx.conf") fl

    return $ ds ^.. traverse . static . _Just . to go
      <> [(path, "/etc/nginx")]
      <> let crt = "/etc/letsencrypt/live/" in [(crt,crt)]

reload :: (MonadReader DampfContext m, MonadIO m) => m ()
reload = do _ <- liftIO $ system "service nginx reload"
            return ()


enableNewDomain 
  :: (MonadReader DampfContext m, MonadIO m) 
  => IsTest -> Text -> DomainSpec -> m ()

enableNewDomain isTest name spec = when (fromMaybe False $ _letsEncrypt spec) $ do
    excode <- shelly $ do
                errExit False $ run_ "certbot-auto"
                     ["certonly","-q","--nginx","--expand","-d",name, "-d","www."`T.append` name]
                lastExitCode
    if (excode ==0)
      then writeNginxConfigFile isTest name spec
      else writeNginxConfigFile isTest name spec { _letsEncrypt = Just False}

domainSslExists :: (MonadReader DampfContext m, MonadIO m) => Text -> m Bool
domainSslExists name = do
    let fnm = ("/etc/nginx/sites-available" </> T.unpack name)
    ex <- liftIO $ doesFileExist fnm
    if not ex
        then return False
        else do fl <- liftIO $ T.readFile fnm
                return $ "ssl_certificate" `T.isInfixOf` fl

writeNginxConfigFile :: (MonadReader DampfContext m, MonadIO m) => IsTest -> Text -> DomainSpec -> m ()
writeNginxConfigFile isTest name spec = do
    fl <- domainConfig isTest name spec

    liftIO $ do
        let strName = T.unpack name

        T.writeFile ("/etc/nginx/sites-available" </> strName) fl
        removePathForcibly ("/etc/nginx/sites-enabled" </> strName)
        createSymbolicLink ("/etc/nginx/sites-available" </> strName)
            ("/etc/nginx/sites-enabled" </> strName)


moveStaticItems :: (MonadReader DampfContext m, MonadIO m) => Text -> Maybe FilePath -> m ()
moveStaticItems s (Just src) = liftIO $ do
    exists <- doesDirectoryExist dest
    when exists $ removeDirectoryRecursive dest

    createDirectoryIfMissing True dest
    void . system $ "cp -R " ++ (src </> ".") ++ " " ++ dest
  where
    dest = "/var/www" </> T.unpack s

moveStaticItems _ Nothing    = return ()
