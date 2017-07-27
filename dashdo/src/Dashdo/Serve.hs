{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, FlexibleContexts, TemplateHaskell #-}

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
import qualified Data.ByteString.Lazy as BLS

import Control.Monad (forM_)

dashdoHandler :: MonadIO m => Dashdo m a -> m ([Param] -> ActionM ())
dashdoHandler d = do
  (_, ff) <- dashdoGenOut d (initial d)
  return $ \ps -> do
         let newval = parseForm (initial d) ff ps
         (thisHtml, _) <- liftIO $ dashdoGenOut d newval
         html thisHtml

getRandomUUID :: IO Text
getRandomUUID = fromStrict . UUID.toText <$> randomIO

dashdoJS :: BLS.ByteString
dashdoJS = BLS.fromStrict $(embedFile "public/js/dashdo.js")

dashdoJSrunnerBase :: BLS.ByteString
dashdoJSrunnerBase = BLS.fromStrict $(embedFile "public/js/runners/base.js")

runDashdo :: MonadIO m => Dashdo m a -> m ()
runDashdo d = do
  (iniHtml, _) <- dashdoGenOut d (initial d)
  h <- dashdoHandler d
  liftIO $ serve iniHtml [("", "", h)]

runRDashdo :: Monad m => Text -> [RDashdo m] -> IO ()  -- TODO: -> m ()
runRDashdo html ds = undefined {- do
  handlers <- mapM (\(RDashdo _ _ d) -> dashdoHandler d) ds
  serve html $ zip3 (map rdFid ds) (map rdTitle ds) handlers
  -}

serve :: Text -> [(String, T.Text, [Param] -> ActionM ())] -> IO ()
serve iniHtml handlers = do
  uuid <- getRandomUUID
  -- this is obviously incorrect (if the form fields change dynamically)
  scotty 3000 $ do
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
    forM_ handlers $ \(did, _, hdl) -> post (literal ('/':did)) (params >>= hdl)
