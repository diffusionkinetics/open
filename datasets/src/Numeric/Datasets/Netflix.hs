{-# language OverloadedStrings, TemplateHaskell #-}
{-|
Netflix prize dataset

From the README :

The movie rating files contain over 100 million ratings from 480 thousand randomly-chosen, anonymous Netflix customers over 17 thousand movie titles.  The data were collected between October, 1998 and December, 2005 and reflect the distribution of all ratings received during this period.  The ratings are on a scale from 1 to 5 (integral) stars. To protect customer privacy, each customer id has been replaced with a randomly-assigned id.  The date of each rating and the title and year of release for each movie id are also provided.

The competition ended on September, 2009, and the dataset was subsequently removed from the public domain by the company. 

We include in this repository a tiny subset of the original dataset for development purposes.

For further information, see <http://netflixprize.com/>.
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



-- | Dataset files. The directories are scanned recursively and their contents are presented as (FilePath, ByteString) pairs

trainingSet :: [(FilePath, ByteString)]
trainingSet = $(embedDir "datafiles/netflix/training/")

testSet :: [(FilePath, ByteString)]
testSet = $(embedDir "datafiles/netflix/test/")

movies :: [(FilePath, ByteString)]
movies = $(embedDir "datafiles/netflix/movies/")



-- * Data types

data Rating = Rating {userId :: UserId,
                      ratingDate :: Day} deriving (Eq, Show)

-- training set
newtype UserId = UserId {unUserId :: Int} deriving Eq
instance Show UserId where show = show . unUserId

data Train = Train {trainRating :: Rating,
                    rating :: Int } deriving (Eq, Show)

-- movies file
newtype MovieId = MovieId {unMovieId :: Int} deriving Eq
instance Show MovieId where show = show . unMovieId

data Movie = Movie { movieId :: MovieId,
                     releaseYear :: Day,
                     movieTitle :: ByteString } deriving (Eq, Show)

-- "qualifying" file (test set)
newtype Test = Test { testRating :: Rating } deriving (Eq, Show)





-- * Netflix dataset parsers

{-The first line of each training set file contains the movie id followed by a
colon.  Each subsequent line in the file corresponds to a rating from a customer
and its date in the following format:

CustomerID,Rating,Date

- MovieIDs range from 1 to 17770 sequentially.
- CustomerIDs range from 1 to 2649429, with gaps. There are 480189 users.
- Ratings are on a five star (integral) scale from 1 to 5.
- Dates have the format YYYY-MM-DD.-}
trainingSetParser :: PT.Parser ByteString (MovieId, [Train])
trainingSetParser = stanza trainRow

{-The test set ("qualifying") file consists of lines indicating a movie id, followed by a colon, and then customer ids and rating dates, one per line for that movie id.
The movie and customer ids are contained in the training set.  Of course the
ratings are withheld. There are no empty lines in the file.-}
testSetParser :: PT.Parser ByteString [(MovieId, [Test])]
testSetParser = many1 (stanza testRow)

{-Movie information is in the following format:

MovieID,YearOfRelease,Title

- MovieID do not correspond to actual Netflix movie ids or IMDB movie ids.
- YearOfRelease can range from 1890 to 2005 and may correspond to the release of
  corresponding DVD, not necessarily its theaterical release.
- Title is the Netflix movie title and may not correspond to 
  titles used on other sites.  Titles are in English.-}
moviesParser :: PT.Parser ByteString [Movie]
moviesParser = parseRows moviesRow




-- * Netflix dataset row type parsers

trainRow :: PT.Parser ByteString Train
trainRow = do
  uid <- decc
  rate <- decc
  d <- date
  let r = Rating (UserId uid) d
  return $ Train r rate
  
testRow :: PT.Parser ByteString Test
testRow = do
  uid <- decc
  d <- date
  let r = Rating (UserId uid) d
  return $ Test r

moviesRow :: PT.Parser ByteString Movie
moviesRow = do
  mo <- decc
  ye <- decc
  title <- takeWhile (inClass "-a-zA-Z0-9 :,&.")
  return $ Movie (MovieId mo) (fromGregorian (fromIntegral ye) 1 1) title

  



-- * Attoparsec parser combinators

parseRows :: PT.Parser ByteString a -> PT.Parser ByteString [a]
parseRows p = many1 (p <* endOfLine)

-- a "stanza" is a block of rows starting starting with the movie ID and a colon. 
stanza :: PT.Parser ByteString a -> PT.Parser ByteString (MovieId, [a])
stanza p = do
  i <- ident <* endOfLine
  pp <- many1 (p <* endOfLine)
  return (MovieId (fromIntegral i), pp)



-- * Attoparsec helpers

date :: PT.Parser ByteString Day
date = do
  (yy:mm:dd:_) <- sepBy decimal dash
  pure $ fromGregorian (fromIntegral yy) mm dd

comma, dash :: Parser Char
comma = char ','
dash = char '-'

decc :: PT.Parser ByteString Int
decc = do
  d <- decimal
  _ <- comma
  return d

ident :: PT.Parser ByteString Integer
ident = do
  i <- decimal
  _ <- char ':'
  pure i





