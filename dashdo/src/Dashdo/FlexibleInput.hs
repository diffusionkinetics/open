{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeFamilies, ExistentialQuantification, FunctionalDependencies, ExtendedDefaultRules, FlexibleContexts, Rank2Types, TemplateHaskell #-}

module Dashdo.FlexibleInput where

import Dashdo.Types
import Lucid
import Data.Text (Text, unpack, pack)
import Data.List
import Control.Monad.RWS.Strict
import Text.Read (readMaybe)
import Lens.Micro
import Lens.Micro.TH
import Prelude hiding (tail)

infixr 0 <<~

class FlexibleInput a where
  type InputVal a
  (<<~) :: Monad m => Lens' s (InputVal a) -> a -> SHtml m s ()

data TextInput = TextInput
  { textInputClasses :: Text
  , textInputPlaceHolder :: Maybe Text
  }

makeFields ''TextInput

textInput :: TextInput
textInput = TextInput "" Nothing

instance FlexibleInput TextInput where
  type InputVal TextInput = Text
  f <<~ (TextInput cs mph) = do
    n <- fresh
    val <- getValue
    let phA = case mph of
               Nothing -> []
               Just ph -> [placeholder_ ph]

    putFormField (n, lensSetter f)
    input_ $ [type_ "text", class_ cs, name_ n, value_ (val ^. f)]++phA

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

select :: [(Text, a)] -> Select a
select opts = Select "" opts

instance (Eq a) => FlexibleInput (Select a) where
  type InputVal (Select a) = a
  f <<~ (Select cs opts) = do
    (val, n) <- freshAndValue
    let ft s t = case lookup t opts of
                   Nothing -> s
                   Just x  -> lensSetter f s x

    putFormField (n, ft)
    select_ [class_ cs, name_ n] $ do
      forM_ opts $ \(optNm, optVal) ->
        if val ^. f == optVal
           then option_ [value_ optNm, selected_ ""] $ toHtml optNm
           else option_ [value_ optNm] $ toHtml optNm

data Checkbox a = Checkbox
  {
    checkboxLabelText :: Text
  , checkboxChecked :: a
  , checkboxUnchecked :: a
  }

makeFields ''Checkbox

checkbox :: Text -> a -> a -> Checkbox a
checkbox = Checkbox

instance (Eq a) => FlexibleInput (Checkbox a) where
  type InputVal (Checkbox a) = a
  f <<~ (Checkbox text vTrue vFalse) = do
    (val, n) <- freshAndValue
    let
      ft s t =
        case t of
          "true" -> lensSetter f s vTrue
          _      -> lensSetter f s vFalse
      checkd = if val ^. f == vTrue then [checked_] else []

    putFormField (n, ft)
    div_ [class_ "checkbox"] $ do
      label_ $ do
        input_ $ [type_ "checkbox", name_ n, value_ "true"] ++ checkd
        toHtml text
    -- if checkbox doesn't supply a value we get this one instead
    input_ [type_ "hidden", name_ n, value_ "false"]
