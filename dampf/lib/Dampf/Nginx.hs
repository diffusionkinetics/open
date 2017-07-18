{-# LANGUAGE OverloadedStrings  #-}

module Dampf.Nginx where

import           Control.Lens
import           Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Process

import           Dampf.AppFile
import           Dampf.ConfigFile
import           Dampf.Nginx.Config


deployDomains :: (HasDampfConfig c, HasDampfApp a) => c -> a -> IO ()
deployDomains cfg a = iforM_ (a ^. domains) $ \n spec -> do
    moveStaticItems (spec ^. static) (T.unpack n)

    let fl = domainConfig cfg n spec
    T.writeFile ("/etc/nginx/sites-available" </> T.unpack n) fl

    removeIfExists ("/etc/nginx/sites-enabled"</> T.unpack n)
    createSymbolicLink ("/etc/nginx/sites-available"</> T.unpack n)
        ("/etc/nginx/sites-enabled" </> T.unpack n)

    void $ system "service nginx reload"


moveStaticItems :: Maybe FilePath -> String -> IO ()
moveStaticItems (Just src) s = do
    exists <- doesDirectoryExist dest
    when exists $ removeDirectoryRecursive dest

    createDirectoryIfMissing True dest
    void . system $ "cp -R " ++ (src </> ".") ++ " " ++ dest
  where
    dest = "/var/www" </> s

moveStaticItems _        _ = return ()


removeIfExists :: FilePath -> IO ()
removeIfExists fp = do
    ex <- doesFileExist fp
    when ex $ removeFile fp

