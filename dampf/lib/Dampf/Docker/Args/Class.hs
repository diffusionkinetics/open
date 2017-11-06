{-# LANGUAGE GADTs #-}
module Dampf.Docker.Args.Class where

import           Data.Text      (Text)
import qualified Data.Text as T

class ToArgs a where
  toArgs :: a -> [String]

{-foldArgs :: (ToArgs a, Foldable t) => t a -> [String]-}
{-foldArgs = foldMap toArgs-}

{-type Name = String-}
{-type Value = Text-}

{-data Arg where-}
  {-Named :: Name -> Value -> Arg -}
  {-Flag  :: Name -> Arg-}

{-instance ToArgs Arg where-}
  {-toArgs (Named n v) = namedTextArg n v-}
  {-toArgs (Flag  n  ) = flagArg' n-}

{-instance ToArgs a => ToArgs (Maybe a) where-}
  {-toArgs (Just a) = toArgs a-}
  {-toArgs Nothing = []-}

flagArg' :: String -> [String]
flagArg' n | null n = []
           | otherwise = ["--" ++ n]

flagArg :: (Show a) => a -> [String]
flagArg v = if null s then [] else [s]
  where
    s = show v

namedArg :: (Show a) => String -> a -> [String]
namedArg n v = ["--" ++ n ++ "=" ++ show v]

namedTextArg :: String -> Text -> [String]
namedTextArg n v = ["--" ++ n ++ "=" ++ T.unpack v]
