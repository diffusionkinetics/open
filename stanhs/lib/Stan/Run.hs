{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Stan.Run where

{- |

Running stan models

e.g.

runStan myModel myData (sample & numSamples .~ 10000)

-}

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
import Lens.Micro.TH

data Sample = Sample {
  _numSamples :: Int
}

data Optimize = Optimize {
  _iterations :: Int
}

makeLenses ''Sample
makeLenses ''Optimize

sample :: Sample
sample = Sample 1000

optimize :: Optimize
optimize = Optimize 2000

class StanOption a where
  type StanReturns a

  runStan :: [Stan] -> [String] -> a -> IO (StanReturns a)

instance StanOption Sample where
  type StanReturns Sample = Map.Map String [Double]

  runStan ss dataLines (Sample ns) = do
    stTmpDir <- getStanTempDirectory
    dataFile <- writeStanDataFile stTmpDir dataLines
    mdlNm <- compileStanModel stTmpDir ss
    withCurrentDirectory stTmpDir $ do
      let cmd = concat ["./", mdlNm, " sample num_samples=", show ns, " data file=", dataFile ] --TODO set output file
      putStrLn cmd
      _ <- system cmd
      readStanSampleOutput "output.csv"

compileStanModel :: FilePath -> [Stan] ->  IO FilePath
compileStanModel stTmpDir ss = do
  let mdlNm = 's': (show $ abs $ hash ss)
      stanFile = stTmpDir </> mdlNm <.> "stan"
  ex <- doesFileExist (stTmpDir </> mdlNm)
  unless ex $ do
    writeFile stanFile $ ppStans ss
    standir <- findStanDir
    withCurrentDirectory standir $ do
      let cmd = "make " ++ stTmpDir </> mdlNm
      _ <- system cmd
      return ()
  return mdlNm

getStanTempDirectory :: IO FilePath
getStanTempDirectory = do
  tmp <- getTemporaryDirectory
  let stTmpDir = tmp </> "stanhs"
  createDirectoryIfMissing False stTmpDir
  return stTmpDir

writeStanDataFile :: FilePath -> [String] -> IO FilePath
writeStanDataFile dir dataLines = do
  let dataNm = 'd': (show $ abs $ hash dataLines)
      dataFile = dir </> dataNm <.> "data.R"
  writeFile dataFile $ unlines dataLines
  return $ dataNm <.> "data.R"


-- |Try to find the CmdStan installation directory, or die
findStanDir :: IO FilePath
findStanDir = do
    sdenv <- lookupEnv "CMDSTAN_HOME"
    case sdenv of
      Just sd -> return sd
      Nothing -> do
        ex <- doesDirectoryExist "/opt/stan"
        if ex
          then return "/opt/stan"
          else die "Environment variable CMDSTAN_HOME or /opt/stan must point to Stan install directory"

readStanSampleOutput :: FilePath -> IO (Map.Map String [Double])
readStanSampleOutput fp = do
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
