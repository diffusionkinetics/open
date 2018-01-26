{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ViewPatterns #-}


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
import           Dampf.AppFile.Types (Port (..))
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
    , _rm       :: Bool
    , _restart  :: RestartPolicy
    , _interactive :: Bool
    , _net      :: Text
    , _hosts    :: Map Text Text -- (domain, ip)
    , _publish  :: [Port]
    , _envs     :: Map Text Text
    , _img      :: Text
    , _cmd      :: Text
    , _volumes  :: [(FilePath, FilePath)]
    , _dns      :: Maybe Text
    } deriving (Eq, Show)

makeClassy ''RunArgs

instance ToArgs RunArgs where
    toArgs r = ["run"]
        <> bool [] ["--rm"] (r ^. rm)
        <> bool [] ["-it"] (r ^. interactive)
        <> flagArg (r ^. detach)
        <> flagArg (r ^. rm)
        <> namedTextArg "name" (r ^. name)
        <> namedArg "restart" (r ^. restart)
        <> namedTextArg "net" (r ^. net)
        <> foldMapOf (hosts .> itraversed . withIndex) hostArgs r
        <> maybe [] (\ip -> ["--dns", T.unpack ip]) (r^.dns)
        <> foldMapOf (volumes . traversed) volArgs r
        <> foldr portArg [] (r ^. publish)
        <> Map.foldrWithKey envArg [] (r ^. envs)
        <> [r ^. img . to T.unpack]
        <> r ^. cmd . to (words . T.unpack)
      where
        volArgs ("",_) = []
        volArgs (_,"") = []
        volArgs (k, v) = ["-v", k <> ":" ++ v ++ ":ro"]

        hostArgs ("",_) = []
        hostArgs (_,"") = []
        hostArgs (k,v) = ["--add-host", T.unpack $ k <> ":" <> v]
        portArg p ps  = ps ++ ["-p", show p]
        envArg n v es = es ++ ["-e", T.unpack n ++ "=" ++ T.unpack v]

unDaemonize :: RunArgs -> RunArgs
unDaemonize =
    set rm True
  . set detach (Detach False)
  . set interactive True
  . set restart No

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

{-mkRunArgsNonDaemon :: (MonadIO m, MonadThrow m)-}
    {-=> Text -> ContainerSpec -> DampfT m RunArgs-}
{-mkRunArgsNonDaemon n spec = do-}
    {-ms <- view (config . postgres)-}
    {-md <- view (app . databases . at (spec ^. useDatabase . non ""))-}

    {-case (ms, md) of-}
        {-(Just s, Just d)  -> return $ args { _envs = es s d }-}
        {-(Just _, Nothing) -> return args-}
        {-_                 -> throwM NoDatabaseServer-}
  {-where-}
    {-args   = (defaultRunArgs n spec) { _rm = Rm True, _detach = Detach False, _restart = No }-}

    {-es s d = Map.fromList-}
        {-[ ("PGHOST", s ^. host)-}
        {-, ("PGPORT", s ^. port . to (T.pack . show))-}
        {-, ("PGDATABASE", spec ^. useDatabase . non "")-}
        {-, ("PGUSER", d ^. user)-}
        {-, ("PGPASSWORD", s ^. users . at (d ^. user) . non "")-}
        {-]-}

defaultRunArgs :: Text -> ContainerSpec -> RunArgs
defaultRunArgs n spec = RunArgs
    { _name     = n
    , _detach   = Detach True
    , _rm       = False
    , _restart  = Always
    , _interactive = False
    , _net      = "host"
    , _publish  = spec ^. expose . non []
    , _envs     = Map.empty
    , _img      = spec ^. image
    , _cmd      = spec ^. command . non ""
    , _hosts    = Map.empty
    , _volumes  = []
    , _dns      = Nothing
    }
