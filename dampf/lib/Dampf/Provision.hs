{-# LANGUAGE OverloadedStrings, DeriveGeneric  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Dampf.Provision (ProvisionType(..), goProvision) where

import Shelly
import Data.Text (Text)
import qualified Data.Text as T
import Dampf.Types
import Control.Monad.Catch      (MonadThrow)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import GHC.Generics

default (Text)

data ProvisionType =
   SingleServer | Development | CI
    deriving (Show, Read, Eq, Generic)

goProvision :: (MonadIO m, MonadThrow m) => ProvisionType -> m ()
goProvision Development = shelly $ core >> docker >> nginx >> postgresql
goProvision _ = shelly $ core >> ufw >> docker >> nginx >> certbot >> postgresql

core :: Sh ()
core = do
  aptUpdate
  aptInstall ["git", "zile", "curl", "lsb-core",
              "software-properties-common"]

docker :: Sh ()
docker = unlessExistsCmd "docker" $
  bash_ "curl" ["https://get.docker.com/ | sh"]

nginx:: Sh ()
nginx = do
  aptInstall ["nginx"]

postgresql :: Sh ()
postgresql = unlessExistsCmd "psql" $ do
  bash_ "add-apt-repository" ["\"deb http://apt.postgresql.org/pub/repos/apt/ $(lsb_release -cs)-pgdg main\""]
  bash_ "wget" ["--quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add"]
  aptUpdate
  aptInstall ["postgresql-10"]

ufw :: Sh ()
ufw = unlessExistsCmd "ufw" $ do
  aptInstall ["ufw"]
  run_ "ufw" ["default","deny","incoming"]
  run_ "ufw" ["default","allow","outgoing"]
  run_ "ufw" ["allow","ssh"]
  run_ "ufw" ["allow","80"]
  run_ "ufw" ["allow","443"]
  run_ "ufw" ["enable"]

certbot :: Sh ()
certbot = unlessExistsCmd "certbot-auto" $ do
  run_ "wget" ["-O","/usr/local/bin/certbot-auto","https://dl.eff.org/certbot-auto"]
  run_ "chmod" ["+x","/usr/local/bin/certbot-auto"]
  run_ "certbot-auto" ["register","--email","dampf@diffusionkinetics.com","--agree-tos","--noninteractive"]
  bash_ "" ["(crontab -l 2>/dev/null ; echo \"42 */12 * * * certbot renew --allow-subset-of-names\") | crontab"]
------------------------------------------------------
--             TOOLS
------------------------------------------------------

existsCmd :: Text -> Sh Bool
existsCmd cmd = do
  errExit False $ run_ "which" [cmd]
  (==0) <$> lastExitCode

unlessExistsCmd :: Text -> Sh () -> Sh ()
unlessExistsCmd cmd go = do
  ex <- existsCmd cmd
  when (not ex) go

aptUpdate :: Sh ()
aptUpdate =
  run_ "apt-get" $ ["-yqq","update"]

aptInstall :: [Text] -> Sh ()
aptInstall pkgs =
  run_ "apt-get" $ "-yqq":"install":pkgs


{-
sudo apt-get -yqq update
sudo apt-get -yqq install curl lsb-core
curl https://get.docker.com/ | sudo sh

sudo apt-get -yqq install nginx

sudo add-apt-repository "deb http://apt.postgresql.org/pub/repos/apt/ $(lsb_release -cs)-pgdg main"
wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add
sudo apt-get -yqq update
sudo apt-get -yqq install postgresql-10
-}