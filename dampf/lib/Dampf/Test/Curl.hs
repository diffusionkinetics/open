{-# language TupleSections #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language OverloadedLists #-}
{-# language ViewPatterns #-}
{-# language FlexibleContexts #-}

module Dampf.Test.Curl where

import Dampf.Types
import Dampf.Test
import Dampf.Monitor
import Dampf.Docker.Free
import Dampf.Docker.Types
import Dampf.Docker.Args.Run hiding (rm)
import Dampf.Nginx (pretendToDeployDomains)

import Data.Text (Text)
import Data.Map (Map)
import Data.Function (on)
import Data.Monoid ((<>))
import Data.Foldable (traverse_, find)
import Text.Regex.Posix

import System.FilePath ((</>))
import System.Exit (exitFailure)
import System.Directory

import Control.Lens 
import Control.Monad 
import Control.Monad.Reader
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

testWithCurl :: (MonadCatch m, MonadIO m) => Tests -> DampfT m ()
testWithCurl ls = do
  netName <- randomName
  proxiedNames <- 
    let proxiedContainerNamesL =
          (app.domains.traversed.proxyContainer._Just.to 
            (head . T.splitOn ":"))
    in ask <&> toListOf proxiedContainerNamesL
  
  testContainers <- tests_to_run ls 

  let 
    testContainerNames = 
      toListOf 
        (traversed.tsUnits.traversed.traverseTestRunImageName) 
        testContainers

    containerNames = 
      nginx_container_name 
      : curlName
      : proxiedNames ++ testContainerNames

    onlyProxyContainers = app.containers.to 
      (Map.filter (^. image.to (flip elem proxiedNames)))

  void . runDockerT $ do
    netCreate netName

    view onlyProxyContainers >>= imapM_ (runWith (set net netName))

    nginxIP <- pretendToDeployDomains >>= runNginx netName

    fakeHosts <- set mapped nginxIP 
      <$> view (app . domains)

    let runArgsTweak =  set net netName 
                      . set detach (Detach False)
                      . set hosts fakeHosts

    runTests' runArgsTweak testContainers

    stopMany containerNames
    void (rmMany containerNames)
    void (netRM [netName])


sendFormData' 
  :: MonadIO m 
  => MonadCatch m
  => (RunArgs -> RunArgs) -> FormData -> DockerT m ()

sendFormData' argsTweak form = do
  void $ runCurl
    argsTweak
    (form ^. formMethod)
    (form ^. formContents)
    (form ^. formAction)

runTests'
  :: (Monad m, MonadReader DampfContext m, MonadIO m, MonadCatch m) 
  => (RunArgs -> RunArgs) 
  -> Map Text TestSpec 
  -> DockerT m ()

runTests' argsTweak = imapM_ go
  where 
    go n (TestSpec us _ mbForm) = do
      maybe (return ()) (sendFormData' argsTweak) mbForm

      report ("running test: " <> T.unpack n) 
      traverse_ (runUnit' argsTweak) us


runUnit'
  :: (MonadReader DampfContext m, MonadIO m, MonadCatch m) 
  => (RunArgs -> RunArgs) 
  -> TestUnit 
  -> DockerT m ()

runUnit' argsTweak = \case
  TestRun name' cmd' -> do
    cs <- view (app . containers)
    find (has $ image . only name') cs & maybe 
      (liftIO exitFailure)
      (void . runWith (set cmd cmd' . argsTweak) name')

  TestGet action mb_pattern -> do
    res <- runCurl argsTweak Get mempty action

    case mb_pattern of
      Nothing -> (report . T.unpack) res
      Just p 
        | ((=~) `on` T.unpack) res p -> 
            report (T.unpack action <> " [OK]")
        | otherwise -> do
            report ("[FAIL] pattern " <> show p <> " didn't match\n") 
            (report . T.unpack) action


curlName :: Text
curlName = "dampf-curl"


runCurl 
  :: MonadIO m 
  => MonadCatch m 
  => (RunArgs -> RunArgs) 
  -> Method 
  -> Map Text Text 
  -> Text 
  -> DockerT m Text

runCurl argsTweak method contents action = go
  where 
    args  = argsTweak
          . set volumes [ (path, path) ]
          . set detach (Detach False)
          . set interactive True
          . set privileged True

    curlSpec isThereJar = 
      ContainerSpec 
        "docker.io/appropriate/curl"
        (Just [Port 80, Port 443])
        (Just 
          ( 
              if isThereJar 
                then " --cookie " <> T.pack (path </> "cookie.dump")
                else mempty

          <>  " --cookie-jar "    <> T.pack (path </> "cookie.dump")
          <>  case method of
                Get -> "\"" <> action <> urlEncode contents
                Post -> Map.foldMapWithKey
                  (\k v -> " -d " <> k <> "=" <> v)
                  contents
                <> " " <> action
          )
        )
        Nothing
    
    path = "/tmp/dampf/curl"

    urlEncode = ("?" <>) . Map.foldMapWithKey (\k v -> k <> "=" <> v)
        
    go = do
      isThereJar <- (liftIO . doesFileExist) (path </> "cookie.dump")
      res <- runWith args curlName (curlSpec isThereJar) 

      _ <- rm curlName
      return res
