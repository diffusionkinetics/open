{-# LANGUAGE MultiParamTypeClasses #-}

module Beetle.Datasets.Streaming where

import qualified Streaming.Prelude as S
import Streaming.Prelude (Stream, Of)
import qualified Data.ByteString.Streaming as SBS
import Streaming.Cassava
import Numeric.Datasets
import Control.Monad.IO.Class
import Data.Maybe
import Control.Monad.Error.Class

unCsvException :: CsvParseException -> String
unCsvException (CsvParseException s) = s

streamDataset :: Dataset a -> Stream (Of (Either String a)) IO ()
streamDataset ds = do
  dir <- liftIO $ tempDirForDataset ds
  lbs <- liftIO $ fmap (fromMaybe id $ preProcess ds) $ getFileFromSource dir $ source ds
  readStreamDataset (readAs ds) $ SBS.fromLazy lbs

readStreamDataset :: ReadAs a -> SBS.ByteString IO () -> Stream (Of (Either String a)) IO ()
readStreamDataset (CSVRecord hhdr opts) sbs
  = fmap (const ()) $ S.map (either (Left . unCsvException) Right) $ decodeWithErrors opts hhdr sbs

foldDataset :: Dataset a -> (b -> Either String a -> IO b) -> b -> IO b
foldDataset ds accf x0 = do
  let s = streamDataset ds
  S.foldM_ accf (return x0) (return) s
-- S.foldM_ :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Stream (Of a) m r -> m b