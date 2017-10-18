{-# LANGUAGE OverloadedStrings, TypeFamilies, ExtendedDefaultRules, TemplateHaskell, DeriveGeneric #-}

module Todo.Items where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Expr
import GHC.Generics
import Prelude hiding (id)
import Data.Aeson

data Todo = Todo
  { id :: Serial Int
  , title :: String
  , done :: Bool
  } deriving (Show, Generic)

instance FromRow Todo
instance ToRow Todo
instance HasFieldNames Todo
instance HasTable Todo where
  tableName _ = "todos"
instance HasKey Todo where
  type Key Todo = Serial Int
  getKey = id
  getKeyFieldNames _ = ["id"]

instance ToJSON Todo