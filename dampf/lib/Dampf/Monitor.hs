{-# language FlexibleContexts #-}
{-# language ViewPatterns #-}
{-# language ScopedTypeVariables  #-}
{-# language LambdaCase  #-}
{-# language OverloadedStrings #-}
{-# language BangPatterns #-}

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
import Data.Foldable (traverse_, find)
import Data.Monoid ((<>))
import Data.Map.Strict (Map)
import Data.ByteString.Lazy.Lens (unpackedChars)

import Text.Regex.Posix

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

import Network.Wreq 
import qualified Network.Wreq.Session as Sess

type IP = Text 
type Tests = [Text]

runMonitor :: (MonadIO m, MonadCatch m) => Tests -> DampfT m ()
runMonitor tests' = runDockerT . runTests id =<< tests_to_run tests'

sendFormData :: MonadIO m => FormData -> m ()
sendFormData form =
  let 
      contents  = form ^. formContents . to Map.toList
      action    = form ^. formAction . to T.unpack
      opts      = defaults & params .~ contents
      go        = BS.pack . T.unpack
      process (a,b) = (go a, go b) 

  in  void $ case (form ^. formMethod) of
        Get  -> (liftIO . getWith opts) action
        Post -> (liftIO . postWith opts action . map process) contents

runTests 
  :: (Monad m, MonadReader DampfContext m, MonadIO m, MonadCatch m) 
  => (RunArgs -> RunArgs) 
  -> Map Text TestSpec 
  -> DockerT m ()

runTests argsTweak = imapM_ go
  where 
    go n (TestSpec us _ mbForm) = do
      sess <- liftIO Sess.newSession

      maybe (return ()) sendFormData mbForm

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

  TestGet action mb_pattern -> do
    res <- (liftIO . Sess.get session) (T.unpack action)
      <&> (^. responseBody . unpackedChars)

    case mb_pattern of
      Nothing -> report res
      Just p 
        | res =~ T.unpack p -> report (T.unpack action <> " [OK]")
        | otherwise -> do
            report ("[FAIL] pattern " <> show p <> " didn't match\n") 
            (report . T.unpack) action


type URL = String


tests_to_run 
  :: MonadReader DampfContext m 
  => Monad m 
  => Tests 
  -> m (Map Text TestSpec)

tests_to_run [] = all_tests 
tests_to_run xs = all_tests 
  <&> Map.filterWithKey (const . flip elem xs)


report :: MonadIO m => String -> m ()
report = liftIO . putStrLn 


all_tests 
  :: MonadReader DampfContext m 
  => Monad m 
  => m (Map Text TestSpec)
all_tests = view l 
  where 
    l = app . tests . to (Map.filter $ not . isOnlyAtBuild)


isOnlyAtBuild :: TestSpec -> Bool
isOnlyAtBuild (TestSpec _ whens _) = [AtBuild] == whens
