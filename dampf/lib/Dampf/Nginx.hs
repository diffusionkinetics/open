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


deployDomains :: (HasDampfConfig c) => c -> Dampfs -> IO ()
deployDomains cfg (Dampfs ss) = forM_ ds $ \(d,dSpec) -> do
    moveStaticItems (dSpec ^. static) d

    let fl = domainConfig cfg (T.pack d) dSpec
    T.writeFile ("/etc/nginx/sites-available" </> d) fl

    removeIfExists ("/etc/nginx/sites-enabled"</> d)
    createSymbolicLink ("/etc/nginx/sites-available"</> d)
        ("/etc/nginx/sites-enabled" </> d)

    void $ system "service nginx reload"
  where
    ds = [(d, dSpec) | Domain d dSpec <- ss]


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

