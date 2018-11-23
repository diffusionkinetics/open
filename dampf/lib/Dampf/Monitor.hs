{-# language ViewPatterns, ScopedTypeVariables, LambdaCase, OverloadedStrings #-}
module Dampf.Monitor where

import Dampf.Docker.Free (runDockerT)
import Dampf.Docker.Types
import Dampf.Docker.Args.Run
import Dampf.Types

import Control.Lens
import Control.Monad            (void)
import Control.Monad.Catch      (MonadCatch)
import Control.Monad.IO.Class   (MonadIO, liftIO)

import System.Exit (exitFailure)
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.Foldable (traverse_, find)
import Data.Monoid ((<>))
import Data.Map.Strict (Map)
import Data.ByteString.Lazy.Lens (unpackedChars)

import Text.Regex.Posix
import Text.Regex
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Network.Wreq 
import qualified Network.Wreq.Session as Sess

type IP = Text 
type Tests = [Text]

runMonitor :: (MonadIO m, MonadCatch m) => Tests -> DampfT m ()
runMonitor tests' = do
  app' <- view app
  runDockerT . runTests app' id =<< tests_to_run tests'


runTests 
  :: (MonadIO m, MonadCatch m) 
  => DampfApp
  -> (RunArgs -> RunArgs) 
  -> Map Text TestSpec 
  -> DockerT m ()

runTests app' argsTweak = imapM_ go
  where 
    go :: (MonadIO m, MonadCatch m) => Text -> TestSpec -> DockerT m ()
    go n (TestSpec us _) = do
      sess <- liftIO Sess.newSession
      report ("running test: " <> T.unpack n) 
      traverse_ (runUnit app' sess argsTweak) us


runUnit 
  :: (MonadIO m, MonadCatch m) 
  => DampfApp
  -> Sess.Session 
  -> (RunArgs -> RunArgs) 
  -> TestUnit 
  -> DockerT m ()

runUnit (view containers -> cs) session argsTweak = \case
  TestRun name' cmd' ->
    find (has $ image . only name') cs & maybe 
      (liftIO exitFailure)
      (void . runWith (set cmd cmd' . argsTweak) name')

  TestGet host mb_pattern -> do
    let hosts' = view hosts (argsTweak emptyArgs)
        uri = (lookupHost hosts' . T.unpack) host

    res <- (liftIO . Sess.get session) uri <&> (^. responseBody . unpackedChars)
    case mb_pattern of
      Nothing -> report res
      Just p 
        | res =~ T.unpack p -> report (uri <> " [OK]")
        | otherwise -> report ("[FAIL] pattern " <> show p <> " didn't match\n") 
                    *> report uri

type URL = String

lookupHost :: Map Text IP -> URL -> URL
lookupHost hosts url = pick . toListOf traverse $ imap (go url) hosts
  where pick (Just url':_) = url'
        pick _ = url

        go :: URL -> Text -> IP -> Maybe URL
        go (T.pack -> url) host ip
          | T.isInfixOf host url = Just . T.unpack $ T.replace host ip url
          | otherwise = Nothing

tests_to_run :: Monad m => Tests -> DampfT m (Map Text TestSpec)
tests_to_run [] = all_tests 
tests_to_run xs = all_tests <&> Map.filterWithKey (const . flip elem xs)

report :: (MonadIO m) => String -> m ()
report = liftIO . putStrLn

reportLn :: (MonadIO m) => String -> m ()
reportLn = liftIO . putStrLn

all_tests :: Monad m => DampfT m (Map Text TestSpec)
all_tests = view $ app . tests . to (Map.filter $ not . isOnlyAtBuild)

isOnlyAtBuild :: TestSpec -> Bool
isOnlyAtBuild (TestSpec _ whens) = [AtBuild] == whens
