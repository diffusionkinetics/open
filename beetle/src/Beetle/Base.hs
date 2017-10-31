module Beetle.Base where

import Data.Aeson
import qualified Network.Wreq as Wreq
import Lens.Micro ((^.))

data Key a = Key { theKey :: String, unKey :: a }

class ToURL a where
  toURL :: a -> String

callAPI :: (ToURL a, FromJSON b) => a -> IO b
callAPI x = do
  rsp <- Wreq.asJSON =<< Wreq.get (toURL x)
  return $  rsp ^. Wreq.responseBody