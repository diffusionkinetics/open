{-# language FlexibleContexts #-}
{-# language ViewPatterns #-}
{-# language ScopedTypeVariables  #-}
{-# language LambdaCase  #-}
{-# language OverloadedStrings #-}

module Dampf.Monitor where

import Dampf.Docker.Free (runDockerT)
import Dampf.Docker.Types
import Dampf.Docker.Args.Run
import Dampf.Types

import Control.Lens
import Control.Monad            (void)
import Control.Monad.Reader.Class (MonadReader)
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
import qualified Data.ByteString.Char8 as BS

import Network.Wreq 
import qualified Network.Wreq.Session as Sess

type IP = Text 
type Tests = [Text]

runMonitor :: (MonadIO m, MonadCatch m) => Tests -> DampfT m ()
runMonitor tests' = runDockerT . runTests id =<< tests_to_run tests'

sendFormData :: MonadIO m => (RunArgs -> RunArgs) -> FormData -> m ()
sendFormData argsTweak form =
  let opts      = defaults & params .~ contents
      contents  = form ^. formContents . to Map.toList
      hs        = argsTweak emptyArgs ^. hosts
      action'   = form ^. formAction . to (lookupHost hs)
  in  void $ case (form ^. formMethod) of
        Get  -> (liftIO . getWith opts) action'
        Post -> (liftIO . post action' . map process) contents
          where
            process (a,b) = (go a, go b) 
            go = BS.pack . T.unpack

runTests 
  :: (Monad m, MonadReader DampfContext m, MonadIO m, MonadCatch m) 
  => (RunArgs -> RunArgs) 
  -> Map Text TestSpec 
  -> DockerT m ()

runTests argsTweak = imapM_ go
  where 
    go n (TestSpec us _ mbForm) = do
      sess <- liftIO Sess.newSession

      maybe (return ()) (sendFormData argsTweak) mbForm

      report ("running test: " <> T.unpack n) 
      traverse_ (runUnit sess argsTweak) us


runUnit 
  :: (MonadReader DampfContext m, MonadIO m, MonadCatch m) 
  => Sess.Session 
  -> (RunArgs -> RunArgs) 
  -> TestUnit 
  -> DockerT m ()

runUnit session argsTweak = \case
  TestRun name' cmd' -> do
    cs <- view (app . containers)
    find (has $ image . only name') cs & maybe 
      (liftIO exitFailure)
      (void . runWith (set cmd cmd' . argsTweak) name')

  TestGet host mb_pattern -> do
    let hs  = argsTweak emptyArgs ^. hosts
        uri = (lookupHost hs . T.unpack) host

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

tests_to_run 
  :: MonadReader DampfContext m 
  => Monad m 
  => Tests 
  -> m (Map Text TestSpec)
tests_to_run [] = all_tests 
tests_to_run xs = all_tests <&> Map.filterWithKey (const . flip elem xs)

report :: (MonadIO m) => String -> m ()
report = liftIO . putStrLn

reportLn :: (MonadIO m) => String -> m ()
reportLn = liftIO . putStrLn

all_tests 
  :: MonadReader DampfContext m 
  => Monad m 
  => m (Map Text TestSpec)
all_tests = view $ app . tests . to (Map.filter $ not . isOnlyAtBuild)

isOnlyAtBuild :: TestSpec -> Bool
isOnlyAtBuild (TestSpec _ whens _) = [AtBuild] == whens
