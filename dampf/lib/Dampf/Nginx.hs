{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLists  #-}
{-# language FlexibleContexts #-}

module Dampf.Nginx where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Text                  (Text)
import           Data.Map.Strict (Map)
import           Data.Monoid ((<>))
import           Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory 
import           System.FilePath ((</>))
import           System.Posix.Files (createSymbolicLink)
import           System.Process (system)

import           Dampf.Nginx.Config
import           Dampf.Nginx.Types 
import           Dampf.Types
import           Shelly (shelly, errExit, run_, lastExitCode)


deployDomains   
  :: (MonadReader DampfContext m, MonadIO m) 
  => m ()
deployDomains = do
  ds <- view (app . domains)

  ifor_ ds $ \name spec -> do
    moveStaticItems name (spec ^. static)
    writeNginxConfigFile False name spec

    ex <- domainSslExists name
    when ex (enableNewDomain False name spec)

    reload


pretendToDeployDomains 
  :: (MonadReader DampfContext m, MonadIO m) 
  => m (Map FilePath FilePath)

pretendToDeployDomains = do
  ds  <- view $ app . domains

  let go = (\n -> (n, "/var/www" </> n))
      path = "/tmp/dampf/test-nginx"
      
      touch dest = do
        shouldntTouch <- doesFileExist (path </> dest)
        unless shouldntTouch $ do
          T.writeFile (path </> dest) mempty
          setPermissions (path </> dest)
            (emptyPermissions { readable = True, writable = True })
        

  liftIO $ do
    createDirectoryIfMissing True (path </> "logs")
    touch "logs/dampf-nginx-access.log"
    touch "logs/dampf-nginx-error.log" 

  confs <- ifor ds (domainToServer True)

  let fl = pShowTestServers (foldMap id confs)
      root = "/tmp/dampf/test-nginx/"

  liftIO $ putStrLn fl
  liftIO $ writeFile (root </> "nginx.conf") fl

  return . Map.fromList $ 
    ds ^.. traverse . static . _Just . to go
    <> [(path, "/etc/nginx")]
    <> let crt = "/etc/letsencrypt/live/" in [(crt,crt)]


reload :: (MonadReader DampfContext m, MonadIO m) => m ()
reload = (void . liftIO . system) "service nginx reload"


enableNewDomain 
  :: (MonadReader DampfContext m, MonadIO m) 
  => IsTest -> Text -> DomainSpec -> m ()

enableNewDomain isTest name spec = 
  when (fromMaybe False $ _letsEncrypt spec) $ do
    excode <- shelly $ do
      errExit False $ run_ "certbot-auto"
        [ "certonly"
        , "-q"
        , "--nginx"
        , "--expand"
        , "-d"
        , name
        ,  "-d"
        , "www."`T.append` name
        ]
      lastExitCode
    if (excode == 0)
      then writeNginxConfigFile isTest name spec
      else writeNginxConfigFile isTest name spec 
        { _letsEncrypt = Just False}


domainSslExists 
  :: (MonadReader DampfContext m, MonadIO m) 
  => Text 
  -> m Bool

domainSslExists name = do
  let fnm = ("/etc/nginx/sites-available" </> T.unpack name)
  ex <- liftIO $ doesFileExist fnm
  if not ex
    then return False
    else do 
      fl <- liftIO $ T.readFile fnm
      return $ "ssl_certificate" `T.isInfixOf` fl


writeNginxConfigFile 
  :: (MonadReader DampfContext m, MonadIO m) 
  => IsTest 
  -> Text 
  -> DomainSpec 
  -> m ()

writeNginxConfigFile isTest name spec = do
  fl <- domainConfig isTest name spec

  liftIO $ do
    let strName = T.unpack name <> ".conf"
        root 
          | isTest    = "/tmp/dampf/test-nginx/"
          | otherwise = "/etc/nginx/"

    createDirectoryIfMissing True $ root </> "sites-available"
    createDirectoryIfMissing True $ root </> "sites-enabled"

    T.writeFile 
      ( root </> "sites-available" </> strName ) fl
    removePathForcibly 
      ( root </> "sites-enabled"   </> strName )
    createSymbolicLink 
      ( root </> "sites-available" </> strName )
      ( root </> "sites-enabled"   </> strName )


moveStaticItems 
  :: (MonadReader DampfContext m, MonadIO m) 
  => Text 
  -> Maybe FilePath 
  -> m ()

moveStaticItems s (Just src) = liftIO $ do
  let dest = "/var/www" </> T.unpack s

  exists <- doesDirectoryExist dest
  when exists $ removeDirectoryRecursive dest

  createDirectoryIfMissing True dest
  void . system $ "cp -R " ++ (src </> ".") ++ " " ++ dest

moveStaticItems _ Nothing = return ()
