{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Dampf.Nginx where

import Dampf.AppFile
import System.Process
import Control.Monad
import System.Exit
import System.Directory
import System.FilePath
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Dampf.Nginx.Config
import System.Posix.Files

deployDomains :: Dampfs -> IO ()
deployDomains (Dampfs dampfs) = do
  forM_ [(nm,dspec) | Domain nm dspec <- dampfs] $ \(nm,dspec) -> do
    --move static items
    case static dspec of
      Nothing -> return ()
      Just sdir -> do
        let dest = "/var/www" </> nm
        ex <- doesDirectoryExist dest
        when ex $ removeDirectoryRecursive dest
        createDirectoryIfMissing True dest
        system $ "cp -R "++(sdir</>".")++" "++dest
        return ()

    --create file
    let fl = domainConfig (T.pack nm) dspec
    T.writeFile ("/etc/nginx/sites-available"</>nm) fl

    removeIfExists ("/etc/nginx/sites-enabled"</>nm)
    createSymbolicLink ("/etc/nginx/sites-available"</>nm) ("/etc/nginx/sites-enabled"</>nm)
    system "service nginx reload"
    return ()

removeIfExists fp = do
  ex <- doesFileExist fp
  when ex $ removeFile fp