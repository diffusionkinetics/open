module Stan.Run where

import Stan.AST
import System.Directory
import System.FilePath
import System.Environment
import System.Directory
import Data.Hashable
import System.Process
import Control.Monad (unless)
import Data.List (transpose)
import qualified Data.Map.Strict as Map

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
    Just envdir <- lookupEnv "STANDIR" -- TODO: Handle Nothing case
    withCurrentDirectory standir $ do
      let cmd = "make " ++ stTmpDir </> mdlNm
      _ <- system cmd
      return ()
  withCurrentDirectory stTmpDir $ do
    let cmd = "./" ++ mdlNm ++ " sample data file=" ++ dataFile
    _ <- system cmd
    readStanOutput "output.csv"


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
