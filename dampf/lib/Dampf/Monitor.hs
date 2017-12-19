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
import Data.Text (Text)
import Data.Foldable (find)
import Data.Monoid ((<>))
import Data.Map.Strict (Map)

import Text.Regex.Posix
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

type Tests = [Text]

runMonitor :: (MonadIO m, MonadThrow m) => Tests -> DampfT m ()
runMonitor = runTests id

runTests :: (MonadIO m, MonadThrow m) => (RunArgs -> RunArgs) -> Tests -> DampfT m ()
runTests argsTweak ls = tests_to_run ls >>= (imapM_ $ \n (TestSpec us _) -> do
    report ("running test: " <> T.unpack n) 
    mapM_ (runUnit argsTweak) us)

runUnit :: (MonadIO m, MonadThrow m) => (RunArgs -> RunArgs) -> TestUnit -> DampfT m ()
runUnit argsTweak = \case
  TestRun iname icmd -> do
    cs <- view (app . containers )
    find (has $ image . only iname) cs & maybe 
      (report $ "image " <> show iname <> " not found")
      (void . runDockerT . runWith (set cmd icmd . argsTweak) iname)

  TestGet uri mb_pattern -> do
    res <- fmap T.unpack . runDockerT . runWith argsTweak "curl" . curlSpec $ uri
    case mb_pattern of
      Nothing -> report res
      Just p
        | res =~ T.unpack p  -> report "matched the pattern"
        | otherwise -> report res *> report "didn't match the pattern"
        
report :: (MonadIO m) => String -> DampfT m ()
report = liftIO . putStrLn

tests_to_run :: Monad m => Tests -> DampfT m (Map Text TestSpec)
tests_to_run [] = all_tests 
tests_to_run xs = all_tests <&> Map.filterWithKey (const . flip elem xs)

all_tests :: Monad m => DampfT m (Map Text TestSpec)
all_tests = view $ app . tests . to (Map.filter $ not . isOnlyAtBuild)

isOnlyAtBuild :: TestSpec -> Bool
isOnlyAtBuild (TestSpec _ whens) = [AtBuild] == whens

curlSpec :: Text -> ContainerSpec
curlSpec url = ContainerSpec 
  "appropriate/curl" Nothing (Just $ "-sS '" <> url <> "'") Nothing
