{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeFamilies, ExistentialQuantification, FunctionalDependencies, ExtendedDefaultRules, FlexibleContexts, Rank2Types, TemplateHaskell #-}

module Dashdo.FlexibleInput where

import Dashdo.Types

import Lucid
import Lucid.Bootstrap
import Lucid.Bootstrap3
import Graphics.Plotly (Plotly)
import Graphics.Plotly.Lucid
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Lazy as TL
import Data.Hashable
import Data.List
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Text.Read (readMaybe)
import Lens.Micro
import Lens.Micro.TH
import Data.Monoid ((<>))
import Prelude hiding (tail)

infixl 0 <<~

class FlexibleInput a where
  type InputVal a
  (<<~) :: Monad m => Lens' s (InputVal a) -> a -> SHtml m s ()

data TextInput = TextInput { textInputClasses :: Text }

makeFields ''TextInput

textInput :: TextInput
textInput = TextInput ""

instance FlexibleInput TextInput where
  type InputVal TextInput = Text
  f <<~ (TextInput cs) = do
    n <- fresh
    val <- getValue

    putFormField (n, lensSetter f)
    input_ [type_ "text", class_ cs, name_ n, value_ (val ^. f)]

data NumInput a = NumInput
  { numInputClasses :: Text
  , numInputMinVal :: Maybe a
  , numInputMaxVal :: Maybe a
  , numInputStep :: Maybe a
  }

makeFields ''NumInput

numInput :: (Show a, Num a) => NumInput a
numInput = NumInput "" Nothing Nothing Nothing

instance (Show a, Read a, Num a) => FlexibleInput (NumInput a) where
  type InputVal (NumInput a) = a
  f <<~ (NumInput cs mmin mmax mstep) = do
    (val,n) <- freshAndValue
    let amin = maybe [] ((:[]) . min_ . pack . show) mmin
        amax = maybe [] ((:[]) . max_ . pack . show) mmax
        astep = maybe [] ((:[]) . step_ . pack . show) mstep
        ft s t = case readMaybe $ unpack t of
                   Nothing -> s
                   Just x -> lensSetter f s x
    putFormField (n, ft)
    input_ (amin  ++ amax ++ astep ++ [type_ "number",
                                       name_ n,
                                       class_ cs,
                                       value_ (pack . show $ val ^. f) ])

data Select a = Select
  {
   selectClasses :: Text
  ,selectOptions :: [(Text, a)]
  }

makeFields ''Select

select opts = Select "" opts

instance (Eq a) => FlexibleInput (Select a) where
  type InputVal (Select a) = a
  f <<~ (Select cs opts) = do
    (val, n) <- freshAndValue
    let ft s t = case lookup t opts of
                   Nothing -> s
                   Just x  -> lensSetter f s x

    putFormField (n, ft)
    select_ [name_ n] $ do
      forM_ opts $ \(optNm, optVal) ->
        if val ^. f == optVal
           then option_ [value_ optNm, selected_ ""] $ toHtml optNm
           else option_ [value_ optNm] $ toHtml optNm  