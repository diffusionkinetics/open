{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, FlexibleContexts, TemplateHaskell, Rank2Types #-}

module Dashdo.Serve where

import Dashdo
import Dashdo.Types
import Dashdo.FileEmbed

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types (status404)
import Control.Monad.Trans (liftIO, MonadIO)
import System.Random
import qualified Data.List as L
import qualified Data.UUID as UUID
import Data.Text.Lazy (Text, fromStrict)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Lazy as BLS

import Control.Monad (forM_)
import Data.Monoid
import Network.HTTP.Types.Status
import Control.Exception
import Data.Hashable
import Lucid
import Control.DeepSeq
import Control.Exception.Safe

type RunInIO m = forall a. m a -> IO a

dashdoHandler :: Monad m => RunInIO m -> Dashdo m a -> IO ([Param] -> ActionM ())
dashdoHandler r d = do
  (iniHtml, ff, _) <- r $ dashdoGenOut d (initial d) []
  print $ hash iniHtml
  return $ \ps -> do
         let newval = parseForm (initial d) ff ps
         (thisHtml, _, _) <- liftIO $ (r $ dashdoGenOut d newval ps) `catchAnyDeep ` (\e -> do
          let es :: String
              es = "<div  class=\"alert alert-danger\" role=\"alert\"><pre>Error: " <> show e<> "</pre></div>"

              foo = TL.pack es <>iniHtml
          return (foo, [], []) )
         html thisHtml

getRandomUUID :: IO Text
getRandomUUID = fromStrict . UUID.toText <$> randomIO

dashdoJS :: BLS.ByteString
dashdoJS = BLS.fromStrict $(embedFile "public/js/dashdo.js")

dashdoJSrunnerBase :: BLS.ByteString
dashdoJSrunnerBase = BLS.fromStrict $(embedFile "public/js/runners/base.js")

runDashdo :: Monad m => RunInIO m -> Dashdo m a -> IO ()
runDashdo = runDashdoPort 3000

runDashdoPort :: Monad m => Int -> RunInIO m -> Dashdo m a -> IO ()
runDashdoPort prt r d = do
  (iniHtml, _, _) <- r $ dashdoGenOut d (initial d) []
  h <- dashdoHandler r d
  serve prt iniHtml [("", "", h)]

runDashdoIO :: Dashdo IO a -> IO ()
runDashdoIO = runDashdoPort 3000 id

runRDashdo :: Monad m => RunInIO m -> Text -> [RDashdo m] -> IO ()
runRDashdo = runRDashdoPort 3000

runRDashdoPort :: Monad m => Int -> RunInIO m -> Text -> [RDashdo m] -> IO ()
runRDashdoPort prt r html ds = do
  handlers <- mapM (\(RDashdo _ _ d) -> dashdoHandler r d) ds
  serve prt html $ zip3 (map rdFid ds) (map rdTitle ds) handlers

serve :: Int -> Text -> [(String, T.Text, [Param] -> ActionM ())] -> IO ()
serve port iniHtml handlers = do
  uuid <- getRandomUUID
  -- this is obviously incorrect (if the form fields change dynamically)
  scotty port $ do
    middleware logStdout
    get "/js/dashdo.js" $ do
      setHeader "Content-Type" "application/javascript"
      raw dashdoJS
    get "/js/runners/:runner" $ do
      runner <- param "runner"
      let runnersEmbedded = $(embedDir "public/js/runners")
      case L.find ((== runner) . fst) runnersEmbedded of
        Just    (_,content) -> do
          setHeader "Content-Type" "application/javascript"
          raw $ BLS.fromStrict content
        Nothing -> do
          status status404

    get "/uuid" $ text uuid
    get "/" $ html iniHtml
    defaultHandler $ \e -> do
      status internalServerError500
      let es = "Dashdo handler error: "<>(TL.pack $ show e)
      liftIO $ TL.putStrLn es
      text (es)
    forM_ handlers $ \(did, _, hdl) -> post (literal ('/':did)) (params >>= hdl)
