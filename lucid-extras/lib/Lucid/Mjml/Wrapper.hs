{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.Mjml.Wrapper where


import Lucid.Mjml.Component

import qualified Data.Text as T

import Data.Semigroup(Semigroup(..))

import qualified Data.HashMap.Strict as HM

import Lucid.Base

import Lucid.Html5

render :: Monad m => HM.HashMap T.Text T.Text -> T.Text -> MjmlT m ()
render 
