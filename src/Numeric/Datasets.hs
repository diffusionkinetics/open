{- |

The datasets package defines two different kinds of datasets:

* small data sets which are directly (or indirectly with `file-embed`)
  embedded in the package as pure values and do not require
  network or IO to download the data set.

* other data sets which need to be fetched over the network with
  `getDataset` and are cached in a local temporary directory

This module defines the `getDataset` function for fetching datasets
and utilies for defining new data sets. It is only necessary to import
this module when using fetched data sets. Embedded data sets can be
imported directly.

-}

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
import qualified Data.Aeson as JSON

import Data.Char (toUpper)
import Text.Read (readMaybe)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.ByteString.Lazy.Search (replace)

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

data Source = URL String

-- |Define a dataset from a pre-processing function and a source for a CSV file
csvDatasetPreprocess :: FromRecord a => (BL.ByteString -> BL.ByteString) -> Source -> Dataset a
csvDatasetPreprocess preF src cacheDir = do
  parseCSV preF <$> getFileFromSource cacheDir src

-- |Define a dataset from a source for a CSV file
csvDataset :: FromRecord a =>  Source -> Dataset a
csvDataset  = csvDatasetPreprocess id

jsonDataset :: JSON.FromJSON a => Source -> Dataset a
jsonDataset src cacheDir = do
  bs <- getFileFromSource cacheDir src
  return $ parseJSON bs

-- | Get a ByteString from the specified Source
getFileFromSource :: FilePath -> Source -> IO (BL.ByteString)
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

parseCSV :: FromRecord a => (BL.ByteString -> BL.ByteString) -> BL.ByteString -> [a]
parseCSV preF contents =
        case decode NoHeader (preF contents) of
          Right theData -> V.toList theData
          Left err -> error err

parseJSON :: JSON.FromJSON a => BL.ByteString -> [a]
parseJSON bs = case JSON.decode bs of
  Just theData ->  theData
  Nothing -> error "failed to parse json"



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

-- | Convert a Fixed-width format to a CSV
fixedWidthToCSV :: BL.ByteString -> BL.ByteString
fixedWidthToCSV = BL8.pack . fnl . BL8.unpack where
  f [] = []
  f (' ':cs) = ',':f (chomp cs)
  f ('\n':cs) = '\n':fnl cs
  f (c:cs) = c:f cs
  fnl cs = f (chomp cs) --newline
  chomp (' ':cs) = chomp cs
  chomp (c:cs) = c:cs
  chomp [] = []
