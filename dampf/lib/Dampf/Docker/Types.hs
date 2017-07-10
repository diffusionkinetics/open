{-# LANGUAGE DeriveFunctor #-}

module Dampf.Docker.Types
  ( -- * Docker Monad
    DockerT
  , DockerF(..)
  ) where

import Control.Monad.Trans.Free (FreeT)


-- Docker Monad

type DockerT = FreeT DockerF


data DockerF next
    = Build String FilePath next
    | Rm String (String -> next)
    | Run String String (Maybe [Int]) (Maybe String) next
    | Stop String next
    deriving (Functor)

