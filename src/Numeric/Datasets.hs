{-# LANGUAGE OverloadedStrings #-}

module Numeric.Datasets where

import Network.HTTP
import Data.Csv
import System.FilePath
import System.Directory
import Data.Hashable
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Data.Char (toUpper)
import Text.Read (readMaybe)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.ByteString.Lazy.Search (replace)

import Paths_datasets (getDataFileName)
-- * Using datasets

-- |Load a dataset, using the system temporary directory as a cache
getDataset :: Dataset a -> IO [a]
getDataset ds = do
  dir <- getTemporaryDirectory
  ds $ dir </> "haskds"

-- | A dataset is defined as a function from the caching directory to the IO action that loads the data
type Dataset a = FilePath -- ^ Directory for caching downloaded datasets
                 -> IO [a]

-- * Defining datasets

data Source = URL String | CabalDataFile FilePath


-- |Define a dataset from a pre-processing function and a URL for a CSV file
csvDatasetPreprocess :: FromRecord a => (BL.ByteString -> BL.ByteString) -> Source -> Dataset a
csvDatasetPreprocess preF src cacheDir = do

  let parseFile contents = do
        case decode NoHeader (preF contents) of
          Right theData -> return $ V.toList theData
          Left err -> fail err

  getFileFromSource cacheDir src >>= parseFile

-- |Define a dataset from URL for a CSV file
csvDataset :: FromRecord a =>  Source -> Dataset a
csvDataset  = csvDatasetPreprocess id

getFileFromSource :: FilePath -> Source -> IO (BL.ByteString)
getFileFromSource _ (CabalDataFile fnm) = do
  fullpath <- getDataFileName fnm
  BL.readFile fullpath
getFileFromSource cacheDir (URL url) = do
  createDirectoryIfMissing True cacheDir
  let fnm = cacheDir </> "ds" <> show (hash url)
      castRequest :: Request String -> Request BL.ByteString
      castRequest r = Request (rqURI r) (rqMethod r) (rqHeaders r) ""

  ex <- doesFileExist fnm
  if ex
     then BL.readFile fnm
     else do
       rsp <- simpleHTTP (castRequest $ getRequest url)
       bs <- getResponseBody rsp
       BL.writeFile fnm bs
       return bs


-- * Helper functions for parsing

-- |Turn dashes to CamlCase
dashToCamelCase :: String -> String
dashToCamelCase ('-':c:cs) = toUpper c : dashToCamelCase cs
dashToCamelCase (c:cs) = c : dashToCamelCase cs
dashToCamelCase [] = []

-- | Parse a field, first turning dashes to CamlCase
parseDashToCamelField :: Read a => Field -> Parser a
parseDashToCamelField s =
  case readMaybe (dashToCamelCase $ unpack s) of
    Just wc -> pure wc
    Nothing -> fail "unknown"

-- | parse somethign, based on its read instance
parseReadField :: Read a => Field -> Parser a
parseReadField s =
  case readMaybe (unpack s) of
    Just wc -> pure wc
    Nothing -> fail "unknown"

-- |Drop lines from a bytestring
dropLines :: Int -> BL.ByteString -> BL.ByteString
dropLines 0 s = s
dropLines n s = dropLines (n-1) $ BL.tail $ BL8.dropWhile (/='\n') s

-- | Turn US-style decimals  starting with a period (e.g. .2) into something Haskell can parse (e.g. 0.2)
fixAmericanDecimals :: BL.ByteString -> BL.ByteString
fixAmericanDecimals = replace ",." (",0."::BL.ByteString)
