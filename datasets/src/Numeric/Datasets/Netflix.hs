{-|
Netflix prize dataset

From the README :

The movie rating files contain over 100 million ratings from 480 thousand randomly-chosen, anonymous Netflix customers over 17 thousand movie titles.  The data were collected between October, 1998 and December, 2005 and reflect the distribution of all ratings received during this period.  The ratings are on a scale from 1 to 5 (integral) stars. To protect customer privacy, each customer id has been replaced with a randomly-assigned id.  The date of each rating and the title and year of release for each movie id are also provided.

The competition ended on September, 2009, and the dataset was subsequently removed from the public domain by the company. 

For further information, see <http://netflixprize.com/>
-}

module Numeric.Datasets.Netflix where

import Numeric.Datasets
import Data.Csv




-- training set

newtype UserId = UserId {unUserId :: Int} deriving (Eq, Show)
data Date = Date Int Int Int deriving (Eq, Show)  -- FIXME use `time`

data TrainingSet = TS {usrId :: UserId, rating :: Int, ratingDate :: Date } deriving (Eq, Show)

-- movies file

newtype MovieId = MovieId {unMovieId :: Int} deriving (Eq, Show)
type Year = Int    -- FIXME use `time`

data Movies = Movie { movieId :: MovieId, releaseYear :: Year, movieTitle :: String } deriving (Eq, Show)
