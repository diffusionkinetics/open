{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, FlexibleInstances #-}

module SumTypeExample where

import Data.Text (Text)
-- import qualified Data.Text as T
import GHC.Generics
import Youido.Types
import Youido.Utils

-- data SumForm = SumItem { x :: Int, y :: Text }
             --  | SumList { listName :: Text }
             -- deriving (Show, Generic)

data Colour = Red | Green | Blue { shade :: Text} deriving (Show, Generic)
instance (Monad m) => FormField m Colour

data Quiz = Quiz
  { name :: Text
  , favouriteColour :: Colour
  -- , reason :: Maybe Text
  } deriving (Show, Generic)

instance (Monad m) => FromForm m Quiz

data QuizR = ShowQuiz
           | EditQuiz
           | UpdateQuiz (Form Quiz)
  deriving (Show, Generic)

instance (Monad m) => FromRequest m QuizR
instance ToURL QuizR
