{-# language OverloadedStrings, TupleSections #-}
module Dampf where

import Control.Lens
import Control.Monad.Catch      (MonadThrow)
import Control.Monad.IO.Class   (MonadIO, liftIO)

import Network.Wreq
import Data.Text (Text, unpack)
import Data.Monoid ((<>))
import Data.Maybe (maybeToList)
import Data.Map.Strict (Map)

import Text.Regex.Posix
import qualified Data.Map.Strict as Map

import Dampf.Docker
import Dampf.Nginx
import Dampf.Postgres
import Dampf.Types
import Dampf.Docker.Free (runDockerT)
import Dampf.Docker.Types (run)

dump :: (MonadIO m) => DampfT m ()
dump = do
    a <- view app
    c <- view config

    liftIO $ do
        putStrLn $ pShowDampfApp a
        putStrLn $ pShowDampfConfig c


goBuild :: (MonadIO m, MonadThrow m) => DampfT m ()
goBuild = do
    setupDB
    buildDocker


goDeploy :: (MonadIO m, MonadThrow m) => DampfT m ()
goDeploy = do
    goBuild

    runMigrations Nothing
    deployDomains


runMonitor :: (MonadIO m, MonadThrow m) => Maybe Text -> DampfT m ()
runMonitor mb_test = tests_to_run >>= mapM_ runUnits where 

  runUnits :: (MonadIO m, MonadThrow m) => (Text, TestSpec) -> DampfT m ()
  runUnits (test_name, TestSpec units _) = do
    report $ "running " <> show test_name
    mapM_ go units
    

  go :: (MonadIO m, MonadThrow m) => TestUnit -> DampfT m ()
  go (TestRun img cmd) = do 
    report $ show cmd
    containers_to_run <- view $ app . containers . to (Map.filter (^. image . to (==img)))
    runDockerT $ mapM_ (run cmd) containers_to_run
  
  go (TestGet uri mb_pattern) = liftIO (get . unpack $ uri) >>= \res ->
    let res_code = view (responseStatus . statusCode) res

    in  report (show uri) *> 
        if res_code /= 200 
          then report ("failed. response code: " <> show res_code)
          else case mb_pattern of
            Nothing -> report "success"
            Just pattern -> if view responseBody res =~ unpack pattern
              then report "success"
              else report ("pattern " <> show pattern <> " didn't match")

  report :: (MonadIO m) => String -> DampfT m ()
  report = liftIO . putStrLn

  tests_to_run :: Monad m => DampfT m [(Text, TestSpec)]
  tests_to_run = case mb_test of
    Nothing -> all_tests <&> Map.toList  
    Just tn -> all_tests <&> map (tn,) . maybeToList . Map.lookup tn

  all_tests :: Monad m => DampfT m (Map Text TestSpec)
  all_tests = view $ app . tests . to (Map.filter $ not . isOnlyAtBuild)

  isOnlyAtBuild :: TestSpec -> Bool
  isOnlyAtBuild (TestSpec _ whens) = [AtBuild] == whens
