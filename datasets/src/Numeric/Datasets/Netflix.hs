{-# language OverloadedStrings, TemplateHaskell #-}
{-|
Netflix prize dataset

From the README :

The movie rating files contain over 100 million ratings from 480 thousand randomly-chosen, anonymous Netflix customers over 17 thousand movie titles.  The data were collected between October, 1998 and December, 2005 and reflect the distribution of all ratings received during this period.  The ratings are on a scale from 1 to 5 (integral) stars. To protect customer privacy, each customer id has been replaced with a randomly-assigned id.  The date of each rating and the title and year of release for each movie id are also provided.

The competition ended on September, 2009, and the dataset was subsequently removed from the public domain by the company. 

We include in this repository a small mock dataset in the same format for development purposes.

For further information, see <http://netflixprize.com/>
-}

module Numeric.Datasets.Netflix where

import Prelude hiding (takeWhile)

import Numeric.Datasets
-- import Data.Csv
import Data.FileEmbed
import Data.ByteString hiding (takeWhile)
import Data.Time (Day, fromGregorian)

import qualified Data.Attoparsec.Internal.Types as PT (Parser) 
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 hiding (takeWhile, inClass)




trainingSet :: [(FilePath, ByteString)]
trainingSet = $(embedDir "datafiles/netflix/training/")

testSet :: [(FilePath, ByteString)]
testSet = $(embedDir "datafiles/netflix/test/")



-- Attoparsec

parseRows :: PT.Parser ByteString a -> PT.Parser ByteString [a]
parseRows p = sepBy p endOfLine <* endOfInput

-- a block of rows starting with an integer identifier
stanza :: PT.Parser ByteString a -> PT.Parser ByteString (Integer, [a])
stanza p = do
  i <- ident
  pp <- many1 (p <* endOfLine)
  return (i, pp)

comma, dash :: Parser Char
comma = char ','
dash = char '-'
decc = do
  d <- decimal
  _ <- comma
  return d

ident :: PT.Parser ByteString Integer
ident = do
  i <- decimal
  _ <- char ':'
  endOfLine
  pure i

trainRow :: PT.Parser ByteString TrainingSet
trainRow = do
  uid <- decc
  rate <- decc
  d <- date
  let r = Rating (UserId uid) d
  return $ TrainSet r rate
  
testRow :: PT.Parser ByteString TestSet
testRow = do
  uid <- decc
  d <- date
  let r = Rating (UserId uid) d
  return $ TestSet r

moviesRow :: PT.Parser ByteString Movie
moviesRow = do
  mo <- decc
  ye <- decc
  title <- takeWhile (inClass "a-z")
  return $ Movie (MovieId mo) (fromGregorian (fromIntegral ye) 1 1) title

date :: PT.Parser ByteString Day
date = do
  (yy:mm:dd:_) <- sepBy decimal dash
  pure $ fromGregorian (fromIntegral yy) mm dd




data Rating = Rating {userId :: UserId,
                      ratingDate :: Day} deriving (Eq, Show)

-- training set

newtype UserId = UserId {unUserId :: Int} deriving (Eq, Show)

data TrainingSet = TrainSet {trainRating :: Rating,
                             rating :: Int } deriving (Eq, Show)

-- movies file

newtype MovieId = MovieId {unMovieId :: Int} deriving (Eq, Show)

data Movie = Movie { movieId :: MovieId,
                     releaseYear :: Day,
                     movieTitle :: ByteString } deriving (Eq, Show)


-- "qualifying" file (test set)

newtype TestSet = TestSet { testRating :: Rating } deriving (Eq, Show)
