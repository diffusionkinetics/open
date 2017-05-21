module Stan.Run where

import Stan.AST
import System.Directory
import System.FilePath
import System.Environment
import Data.Hashable
import System.Process
import Control.Monad (unless)
import Data.List (transpose)
import qualified Data.Map.Strict as Map
import System.Exit (die)

runStan :: [Stan] -> [String] -> IO (Map.Map String [Double])
runStan ss dataLines = do
  tmp <- getTemporaryDirectory
  let stTmpDir = tmp </> "stanhs"
      mdlNm = 's': (show $ abs $ hash ss)
      dataNm = 'd': (show $ abs $ hash dataLines)
      stanFile = stTmpDir </> mdlNm <.> "stan"
      dataFile = stTmpDir </> dataNm <.> "data.R"
  createDirectoryIfMissing False stTmpDir
  writeFile dataFile $ unlines dataLines

  ex <- doesFileExist (stTmpDir </> mdlNm)
  unless ex $ do
    writeFile stanFile $ ppStans ss
    standir <- findStanDir
    withCurrentDirectory standir $ do
      let cmd = "make " ++ stTmpDir </> mdlNm
      _ <- system cmd
      return ()
  withCurrentDirectory stTmpDir $ do
    let cmd = "./" ++ mdlNm ++ " sample data file=" ++ dataFile --TODO set output file
    _ <- system cmd
    readStanOutput "output.csv"

-- |Try to find the CmdStan installation directory, or die
findStanDir :: IO FilePath
findStanDir = do
    sdenv <- lookupEnv "CMDSTAN_HOME" -- TODO: Handle Nothing case
    case sdenv of
      Just sd -> return sd
      Nothing -> do
        ex <- doesDirectoryExist "/opt/stan"
        if ex
          then return "/opt/stan"
          else die "Environment variable CMDSTAN_HOME or /opt/stan must point to Stan install directory"

readStanOutput :: FilePath -> IO (Map.Map String [Double])
readStanOutput fp = do
  let noHash ('#':_) = False
      noHash _ = True
  (hdrLn:lns) <- fmap (filter noHash . lines) $ readFile fp
  let hdrs = splitBy ',' hdrLn
      samples = transpose $ map (map read . splitBy ',') lns
  return $ Map.fromList $ zip hdrs samples

splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]]
          where f c l@(x:xs) | c == delimiter = []:l
                             | otherwise = (c:x):xs
