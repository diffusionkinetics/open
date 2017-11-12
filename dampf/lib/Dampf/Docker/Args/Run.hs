{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}


module Dampf.Docker.Args.Run where

import           Control.Lens
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Text                  (Text)
import           Data.Bool (bool)
import qualified Data.Text as T

import           Dampf.Docker.Args.Class
import           Dampf.Types

-- Argument Types for "docker run"

newtype Detach = Detach Bool
    deriving (Eq)


instance Show Detach where
    show (Detach b) = if b then "-d" else ""


data RestartPolicy
    = No
    | Failure
    | Always
    deriving (Eq)


instance Show RestartPolicy where
    show No      = "no"
    show Failure = "failure"
    show Always  = "always"


data RunArgs = RunArgs
    { _name     :: Text
    , _detach   :: Detach
    , _restart  :: RestartPolicy
    , _net      :: Text
    , _publish  :: [Int]
    , _envs     :: Map Text Text
    , _rmArg    :: Bool
    , _img      :: Text
    , _cmd      :: Text
    } deriving (Eq, Show)

makeClassy ''RunArgs


instance ToArgs RunArgs where
    toArgs r = ["run"]
        <> bool ["--rm"] [] (r ^. rmArg)
        <> flagArg (r ^. detach)
        <> namedTextArg "name" (r ^. name)
        <> namedArg "restart" (r ^. restart)
        <> namedTextArg "net" (r ^. net)
        <> foldr portArg [] (r ^. publish)
        <> Map.foldrWithKey envArg [] (r ^. envs)
        <> [r ^. img . to T.unpack]
        <> r ^. cmd . to (words . T.unpack)
      where
        portArg p ps  = ps ++ ["-p", show p ++ ":" ++ show p]
        envArg n v es = es ++ ["-e", T.unpack n ++ "=" ++ T.unpack v]


mkRunArgs :: (MonadIO m, MonadThrow m)
    => Text -> ContainerSpec -> DampfT m RunArgs
mkRunArgs n spec = do
    ms <- view (config . postgres)
    md <- view (app . databases . at (spec ^. useDatabase . non ""))

    case (ms, md) of
        (Just s, Just d)  -> return $ args { _envs = es s d }
        (Just _, Nothing) -> return args
        _                 -> throwM NoDatabaseServer
  where
    args   = defaultRunArgs n spec

    es s d = Map.fromList
        [ ("PGHOST", s ^. host)
        , ("PGPORT", s ^. port . to (T.pack . show))
        , ("PGDATABASE", spec ^. useDatabase . non "")
        , ("PGUSER", d ^. user)
        , ("PGPASSWORD", s ^. users . at (d ^. user) . non "")
        ]

defaultRunArgs :: Text -> ContainerSpec -> RunArgs
defaultRunArgs n spec = RunArgs
    { _name     = n
    , _detach   = Detach True
    , _restart  = Always
    , _net      = "host"
    , _publish  = spec ^. expose . non []
    , _rmArg    = True
    , _envs     = Map.empty
    , _img      = spec ^. image
    , _cmd      = spec ^. command . non ""
    } 
