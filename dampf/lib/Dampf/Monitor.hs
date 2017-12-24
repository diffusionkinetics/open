{-# language LambdaCase, OverloadedStrings, TupleSections #-}
module Dampf.Monitor where

import Dampf.Docker.Free (report, runDockerT)
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
import Data.Foldable (find)
import Data.Monoid ((<>))
import Data.Map.Strict (Map)

import Text.Regex.Posix
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

type Tests = [Text]
type Names = [Text]

runMonitor :: (MonadIO m, MonadThrow m) => Tests -> DampfT m ()
runMonitor = void . runTests id

runTests :: (MonadIO m, MonadThrow m) => (RunArgs -> RunArgs) -> Tests -> DampfT m Names
runTests argsTweak ls = tests_to_run ls >>= fmap (foldOf traverse) . (imapM $ \n (TestSpec us _) -> do
    report ("running test: " <> T.unpack n) 
    traverse (runUnit argsTweak) us)

runUnit :: (MonadIO m, MonadThrow m) => (RunArgs -> RunArgs) -> TestUnit -> DampfT m Text
runUnit argsTweak = \case
  TestRun iname icmd -> do
    cs  <- view (app . containers )
    find (has $ image . only iname) cs & maybe 
      (liftIO exitFailure)
      (runDockerT . runWith (set cmd icmd . argsTweak) iname)
    return iname

  TestGet uri mb_pattern -> do
    (res) <- runDockerT . runWith argsTweak curl_container_name . curlSpec $ uri
    case mb_pattern of
      Nothing -> report (T.unpack res)
      Just p 
        | ((=~) `on` T.unpack) res p -> report "matched the pattern"
        | otherwise -> report "didn't match the pattern" *> report (T.unpack res)
    return curl_container_name
        
tests_to_run :: Monad m => Tests -> DampfT m (Map Text TestSpec)
tests_to_run [] = all_tests 
tests_to_run xs = all_tests <&> Map.filterWithKey (const . flip elem xs)

all_tests :: Monad m => DampfT m (Map Text TestSpec)
all_tests = view $ app . tests . to (Map.filter $ not . isOnlyAtBuild)

isOnlyAtBuild :: TestSpec -> Bool
isOnlyAtBuild (TestSpec _ whens) = [AtBuild] == whens

curl_container_name = "dampf-curl"

curlSpec :: Text -> ContainerSpec
curlSpec url = ContainerSpec 
  "appropriate/curl" Nothing (Just $ "-sS " <> url) Nothing
