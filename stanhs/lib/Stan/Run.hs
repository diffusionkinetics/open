{- |

Running Stan models

Before invoking `runStan`, you must specify where to find the [cmdstan](http://mc-stan.org/interfaces/cmdstan) install directory.
Either set the CMDSTAN_HOME environment variable or symlink or use `/opt/stan`

Simple MCMC sampling:

@
runStan myModel myData sample
@

Changing sampling parameters:

@
runStan myModel myData sample { numSamples = 10000 }
@

Optimizing:

@
runStan myModel myData optimize
@

Optimizing with Newton's method:

@
runStan myModel myData optimize {method = Newton}
@

-}

{-# LANGUAGE TypeFamilies #-}

module Stan.Run (runStan, Sample (..), Optimize (..), sample, optimize, StanMethod (..), OptMethod (..)) where

import Stan.AST
import System.Directory
import System.FilePath
import System.Environment
import Data.Hashable
import System.Process
import Control.Monad (unless)
import Data.List (transpose)
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import System.Exit
import System.Random (randomRIO)

-- | Parameters for the sample method.
--   runStan called with this method will return @Map.Map String [Double]@,
--   with one element in each list per sample from the MCMC chain
data Sample = Sample
  { numSamples :: Int
  , numWarmup :: Int
  , thin :: Int
  }

-- | Parameters for the variational method.
-- runStan called with this method will return @Map.Map String Double@
data Optimize = Optimize
  { iterations :: Int
  , method :: OptMethod
  }

data OptMethod = BFGS | LBFGS | Newton deriving (Show, Eq)

-- | Default value for Sample -- call Stan with the "sample" method
sample :: Sample
sample = Sample 1000 1000 1

-- | Default value for Optimize -- call Stan with the "optimize" method
optimize :: Optimize
optimize = Optimize 2000 LBFGS

-- | The class of stan inference methods
class StanMethod a where
  type StanReturns a

  runStanFiles :: FilePath -> FilePath -> a -> IO (StanReturns a)

instance StanMethod Sample where
  type StanReturns Sample = Map.Map String [Double]

  runStanFiles mdlNm dataFile (Sample ns nw thn) = do
      outFl <- fmap (\i -> concat ["out", show i, ".csv"]) $ randomRIO (1::Int, 99999999)
      let optArg = toArgs [ ("num_samples", show ns)
                          , ("num_warmup", show nw)
                          , ("thin", show thn)
                          ]
          cmd = concat ["./", mdlNm, " sample ", optArg, " data file=", dataFile, " output file=", outFl ] --TODO set output file
      ExitSuccess <- system cmd
      readStanSampleOutput outFl

instance StanMethod Optimize where
  type StanReturns Optimize = Map.Map String Double

  runStanFiles mdlNm dataFile (Optimize iter meth) = do
      outFl <- fmap (\i -> concat ["out", show i, ".csv"]) $ randomRIO (1::Int, 99999999)
      let optArg = toArgs [ ("iter", show iter)
                          , ("algorithm", map toLower $ show meth)
                          ]
          cmd = concat ["./", mdlNm, " optimize ", optArg, " data file=", dataFile, " output file=", outFl ] --TODO set output file
      ExitSuccess <- system cmd
      readStanOptimizeOutput outFl

toArgs :: [(String, String)] -> String
toArgs = unwords . map f where
  f (k,v) = k ++ "=" ++ v

-- | Run a stan model
runStan :: StanMethod a
           => [Stan] -- ^ The Stan model
           -> [String] -- ^ The data file, written using dumpAs
           -> a -- ^ the method to run with
           -> IO (StanReturns a) -- ^ the output from that method
runStan stans dataLines meth = do
    stTmpDir <- getStanTempDirectory
    dataFile <- writeStanDataFile stTmpDir dataLines
    mdlNm <- compileStanModel stTmpDir stans
    withCurrentDirectory stTmpDir $ do
      runStanFiles mdlNm dataFile meth

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
  (hdrs, samples) <- rawStanOutput fp
  return $ Map.fromList $ zip hdrs $ transpose samples

readStanOptimizeOutput :: FilePath -> IO (Map.Map String Double)
readStanOptimizeOutput fp = do
  (hdrs, [samples]) <- rawStanOutput fp
  return $ Map.fromList $ zip hdrs samples

rawStanOutput :: FilePath -> IO ([String], [[Double]])
rawStanOutput fp = do
  let noHash ('#':_) = False
      noHash _ = True
  (hdrLn:lns) <- fmap (filter noHash . lines) $ readFile fp
  let hdrs = splitBy ',' hdrLn
      samples = map (map read . splitBy ',') lns
  return (hdrs, samples)

splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]]
          where f c l@(x:xs) | c == delimiter = []:l
                             | otherwise = (c:x):xs
