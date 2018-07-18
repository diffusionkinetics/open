{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, FlexibleInstances #-}

module SumTypeExample where

import Data.Text (Text)
-- import qualified Data.Text as T
import GHC.Generics
import Youido.Types
import Youido.Utils

data Colour = Red
            | Green { dark :: Bool }
            | Blue { shade :: Text, extra :: Text }
  deriving (Show, Generic)

instance (Monad m) => FormField m Colour

data Reason = PreferNotToSay | Explanation { explanation :: Text }
  deriving (Show, Generic)
instance (Monad m) => FormField m Reason

data Quiz = Quiz
  { name :: Text
  , favouriteColour :: Colour
  , reason :: Reason
  } deriving (Show, Generic)

instance (Monad m) => FromForm m Quiz

data QuizR = ShowQuiz
           | NewQuiz
           | EditQuiz
           | UpdateQuiz (Form Quiz)
  deriving (Show, Generic)

instance (Monad m) => FromRequest m QuizR
instance ToURL QuizR
