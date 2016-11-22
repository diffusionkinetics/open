{-# LANGUAGE OverloadedStrings #-}

module Numeric.Dataset.UCI where

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

type Dataset a = FilePath -- ^ Directory for caching downloaded datasets
                 -> IO [a]
type URL = String

-- |Retrieve a dataset
getDataset :: Dataset a -> IO [a]
getDataset ds = do
  dir <- getTemporaryDirectory
  ds $ dir </> "haskds"

csvDatasetDropLines :: FromRecord a => Int -> URL -> Dataset a
csvDatasetDropLines dlns url cacheDir = do
  createDirectoryIfMissing True cacheDir
  let fnm = cacheDir </> "ds" <> show (hash url)
      parseFile contents = do
        case decode NoHeader (dropLines dlns contents) of
          Right theData -> return $ V.toList theData
          Left err -> fail err
      castRequest :: Request String -> Request BL.ByteString
      castRequest r = Request (rqURI r) (rqMethod r) (rqHeaders r) ""

  ex <- doesFileExist fnm
  if ex
     then BL.readFile fnm >>= parseFile
     else do
       rsp <- simpleHTTP (castRequest $ getRequest url)
       bs <- getResponseBody rsp
       BL.writeFile fnm bs
       parseFile bs

csvDataset :: FromRecord a =>  URL -> Dataset a
csvDataset  = csvDatasetDropLines 0

dashToCamelCase :: String -> String
dashToCamelCase ('-':c:cs) = toUpper c : dashToCamelCase cs
dashToCamelCase (c:cs) = c : dashToCamelCase cs
dashToCamelCase [] = []

parseDashToCamelField :: Read a => Field -> Parser a
parseDashToCamelField s =
  case readMaybe (dashToCamelCase $ unpack s) of
    Just wc -> pure wc
    Nothing -> fail "unknown"

parseReadField :: Read a => Field -> Parser a
parseReadField s =
  case readMaybe (unpack s) of
    Just wc -> pure wc
    Nothing -> fail "unknown"

dropLines :: Int -> BL.ByteString -> BL.ByteString
dropLines 0 s = s
dropLines n s = dropLines (n-1) $ BL.tail $ BL8.dropWhile (/='\n') s
