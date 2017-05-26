{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, FlexibleContexts, TemplateHaskell #-}

module Dashdo.Serve where

import Dashdo
import Dashdo.Types
import Dashdo.FileEmbed

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Control.Monad.Trans (liftIO)
import System.Random
import qualified Data.UUID as UUID
import Data.Text.Lazy (Text, fromStrict)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BLS
import Data.Aeson hiding (json)

import Control.Monad (forM_)

data DashdoLink = DashdoLink String T.Text

instance ToJSON DashdoLink where
    toJSON (DashdoLink did title) = object ["did" .= did, "title" .= title]

dashdoHandler :: Dashdo a -> IO ([Param] -> ActionM ())
dashdoHandler d = do
  (_, ff) <- dashdoGenOut d (initial d)
  return $ \ps -> do
         let newval = parseForm (initial d) ff ps
         (thisHtml, _) <- liftIO $ dashdoGenOut d newval
         html thisHtml

runDashdo :: Dashdo a -> IO ()
runDashdo d = do
  (iniHtml, _) <- dashdoGenOut d (initial d)
  h <- dashdoHandler d
  serve iniHtml [("", "", h)]

runRDashdo :: Text -> [RDashdo] -> IO ()
runRDashdo html ds = do
  handlers <- mapM (\(RDashdo _ _ d) -> dashdoHandler d) ds
  serve html $ zip3 (map rdFid ds) (map rdTitle ds) handlers

serve :: Text -> [(String, T.Text, [Param] -> ActionM ())] -> IO ()
serve iniHtml handlers = do
  uuid <- fromStrict . UUID.toText <$> randomIO
  -- this is obviously incorrect (if the form fields change dynamically)
  let titles = map (\(did, title, _) -> DashdoLink did title) handlers
  scotty 3000 $ do
    middleware logStdout
    get "/js/dashdo.js" $ do
      setHeader "Content-Type" "application/javascript"
      raw $ BLS.fromStrict $(embedFile "public/js/dashdo.js")
    get "/uuid" $ text uuid
    get "/" $ html iniHtml
    get "/titles" $ json titles
    forM_ handlers $ \(did, _, hdl) -> post (literal ('/':did)) (params >>= hdl)
