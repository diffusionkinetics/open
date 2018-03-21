{-# language LambdaCase, OverloadedStrings, TupleSections #-}
module Dampf.Monitor where

import Dampf.Docker.Free (runDockerT)
import Dampf.Docker.Types
import Dampf.Types
import Dampf.Docker.Args.Run

import Control.Lens
import Control.Monad            (void)
import Control.Monad.Catch      (MonadThrow)
import Control.Monad.IO.Class   (MonadIO, liftIO)

import System.Exit (exitFailure)
import Data.Function (on)
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.Foldable (find)
import Data.Monoid ((<>))
import Data.Map.Strict (Map)

import Text.Regex.Posix
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL

import Network.Wreq 

type IP = Text 
type Tests = [Text]
type Names = [Text]
type Hosts = Map Text IP

runMonitor :: (MonadIO m, MonadThrow m) => Tests -> DampfT m ()
runMonitor = void . runTests mempty id

runTests :: (MonadIO m, MonadThrow m) => Hosts -> (RunArgs -> RunArgs) -> Tests -> DampfT m Names
runTests hosts argsTweak ls = tests_to_run ls >>= fmap (catMaybes . foldOf traverse) . imapM go
  where 
    go :: (MonadIO m, MonadThrow m) => Text -> TestSpec -> DampfT m [Maybe Text]
    go n (TestSpec us _) = do
      report ("running test: " <> T.unpack n) 
      traverse (runUnit hosts argsTweak) us

runUnit :: (MonadIO m, MonadThrow m) => Hosts -> (RunArgs -> RunArgs) -> TestUnit -> DampfT m (Maybe Text)
runUnit hosts argsTweak = \case
  TestRun iname icmd -> do
    cs <- view (app . containers)
    find (has $ image . only iname) cs & maybe 
      (liftIO exitFailure)
      (runDockerT . runWith (set cmd icmd . argsTweak) iname)
    pure $ Just iname

  -- make it work on fake hosts
  TestGet uri mb_pattern -> do
    res <- (liftIO . get . T.unpack $ uri) <&> (^. responseBody)
    case mb_pattern of
      Nothing -> report (BL.unpack res)
      Just p 
        | ((=~) `on` BL.unpack) res (BL.pack . T.unpack $ p) -> success
        | otherwise -> report ("[FAIL] pattern " <> show p <> " didn't match") *> report (BL.unpack res)
    pure Nothing

type URL = String
lookupHost :: Hosts -> URL -> Maybe URL
lookupHost hosts url | null hosts = Nothing

tests_to_run :: Monad m => Tests -> DampfT m (Map Text TestSpec)
tests_to_run [] = all_tests 
tests_to_run xs = all_tests <&> Map.filterWithKey (const . flip elem xs)

report :: (MonadIO m) => String -> DampfT m ()
report = liftIO . putStr

reportLn :: (MonadIO m) => String -> DampfT m ()
reportLn = liftIO . putStrLn

success :: (MonadIO m) => DampfT m ()
success = reportLn " [OK]"

reportFail :: (MonadIO m) => String -> DampfT m ()
reportFail s = reportLn s *> liftIO exitFailure

all_tests :: Monad m => DampfT m (Map Text TestSpec)
all_tests = view $ app . tests . to (Map.filter $ not . isOnlyAtBuild)

isOnlyAtBuild :: TestSpec -> Bool
isOnlyAtBuild (TestSpec _ whens) = [AtBuild] == whens
