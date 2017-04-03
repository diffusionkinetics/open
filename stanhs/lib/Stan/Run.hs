module Stan.Run where

import Stan.AST
import System.Directory
import System.FilePath
import System.Environment
import System.Directory
import Data.Hashable
import System.Process
import Control.Monad (unless)

runStan :: [Stan] -> [String] -> IO ()
runStan ss dataLines = do
  tmp <- getTemporaryDirectory
  let stTmpDir = tmp </> "stanhs"
      mdlNm = 's': (show $ abs $ hash ss)
      dataNm = 'd': (show $ abs $ hash dataLines)
      stanFile = stTmpDir </> mdlNm <.> "stan"
      dataFile = stTmpDir </> dataNm <.> "data.R"
  createDirectoryIfMissing False stTmpDir
  writeFile dataFile $ unlines dataLines

  ex <- doesFileExist (stTmpDir</> mdlNm)
  unless ex $ do
    writeFile stanFile $ ppStans ss
    Just standir <- lookupEnv "STANDIR"
    withCurrentDirectory standir $ do
      let cmd = "make "++stTmpDir </> mdlNm
      _ <- system cmd
      return ()
  withCurrentDirectory stTmpDir $ do
    let cmd = "./"++mdlNm ++ " sample data file="++dataFile
    _ <- system cmd
    return ()
