{-# language TupleSections, BangPatterns, LambdaCase, OverloadedStrings, ViewPatterns #-}
module Dampf.Test where

import Dampf.Types
import Dampf.Docker.Free
import Dampf.Docker.Types
import Dampf.Docker.Args.Run
import Dampf.Docker.Args.Class
import Dampf.Nginx.Test

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Monoid ((<>))

import System.Exit (exitFailure)
import Data.Function (on)
import Data.Text (Text)
import Data.Foldable (find)
import Data.Monoid ((<>))
import Data.Map.Strict (Map)

import Text.Regex.Posix

import System.Random

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Catch      (MonadThrow, finally)
import Control.Monad.IO.Class   (MonadIO, liftIO)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

type IP = Text
type Network = Text
type Volumes = [(FilePath, FilePath)]
type Names = [Text]
type Tests = [Text]

-- add database related opearitons to `test`
-- finalize on exceptions

test :: (MonadIO m, MonadThrow m) => Tests -> DampfT m ()
test ls = do
  netName <- randomName
  runDockerT $ netCreate netName

  proxie_names <- runProxies netName

  nginx_ip <- pretendToDeployDomains >>= runNginx netName
  liftIO $ print nginx_ip

  fakeHosts <- set mapped nginx_ip <$> view (app . domains)

  let argsTweak = set net netName
                . set detach (Detach False)
                . set hosts fakeHosts

  test_container_names <- runTests argsTweak ls
  cleanUp netName (nginx_container_name  : (proxie_names ++ test_container_names))
  return ()

nginx_container_name = "dampf-nginx"

runProxies :: (MonadIO m, MonadThrow m) => Network -> DampfT m [Text]
runProxies netName = do
  let pxs = app . domains . traversed . proxyContainer . _Just . to (head . T.splitOn ":")

  proxies <- ask <&> toListOf pxs

  let cts = app . containers . to (Map.filter
        (^. image . to (flip elem proxies)))

  names <- view cts >>= imapM (\n ->
    runDockerT . fmap (const n) . runWith (set net netName) n)

  return (toListOf traverse names)

runNginx :: (MonadIO m, MonadThrow m) => Network -> Volumes -> DampfT m IP
runNginx netName vs = runDockerT $
  runWith xargs nginx_container_name xSpec >>= getIp . T.take 12
    where
      getIp (head . T.lines -> !id') = head . T.lines <$> inspect
        "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}" id'

      xargs = set net netName
            . set volumes vs
            . set publish [Port 433, Port 80]
            {-. set detach (Detach False)-}

      xSpec = ContainerSpec "nginx" Nothing Nothing Nothing

cleanUp :: (MonadIO m, MonadThrow m) => Network -> Names -> DampfT m ()
cleanUp netName names = void . runDockerT $ do
  stopMany names
  rmMany names
  netRM [netName]

randomName :: (MonadIO m, MonadThrow m) => DampfT m Network
randomName = fmap T.pack . replicateM 16 . liftIO . randomRIO $ ('a','z')


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
        | ((=~) `on` T.unpack) res p -> success
        | otherwise -> report ("[FAIL] pattern " <> show p <> " didn't match") *> report (T.unpack res)
    return curl_container_name

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

curl_container_name = "dampf-curl"

curlSpec :: Text -> ContainerSpec
curlSpec url = ContainerSpec
  "appropriate/curl" Nothing (Just $ "-sS " <> url) Nothing
