{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language TypeOperators #-}
{-# language ViewPatterns #-}
module Main where

import Servant
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Cli
import Data.Text (Text)

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as T

main = defWaiMain (serve boilerPlate server)

data User = User
  { login :: Text
  , password :: Text
  } deriving (Eq, Show)

type TestAPI = 
        "post-data" :> ReqBody '[FormUrlEncoded] [(Text, Text)] 
                    :> Post '[PlainText] Text
  :<|>  "get-data"  :> QueryParam "message" Text 
                    :> Get '[PlainText] Text
  :<|>  "test"      :> Get '[PlainText] Text

boilerPlate :: Proxy TestAPI
boilerPlate = Proxy

server :: Server TestAPI
server = postH :<|> getH :<|> messageH
  where
    postH (List.lookup "message" -> Just msg) = go msg
    postH _ = return "something went wrong"

    getH (Just msg) = go msg
    getH _ = return "something went wrong"
    
    go msg = liftIO $ do
      T.writeFile "/data/message.txt" msg
      return "success"

    messageH = (liftIO . T.readFile) "/data/message.txt"
