{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, TemplateHaskell #-}
module Dashdo.Examples.TestDashdo where

import Numeric.Datasets.Iris

import Dashdo
import Dashdo.Types
import Dashdo.Serve
import Dashdo.Elements
import Control.Monad
import Control.Monad.State.Strict
import Lucid
import Data.Monoid ((<>))
import Data.Text (Text, unpack, pack)
import Lens.Micro.Platform

import Graphics.Plotly (plotly, layout, title, Trace)
import Graphics.Plotly.Lucid
import Graphics.Plotly.GoG
import Graphics.Plotly.Histogram (histogram)

data Example = Example
 { _pname :: Text
 , _isMale :: Bool
 , _xaxis :: Tag (Iris -> Double)
 , _yaxis :: Tag (Iris -> Double)
 }

makeLenses ''Example

testDashdo = runDashdoIO $ Dashdo initv (example iris)

test :: SHtml IO Bool ()
test = do
  b <- getValue
  "The person is male: "
  if b then "yes" else "no"

hello :: SHtml IO Text ()
hello = do
  textInput id
  br_ []
  txt  <- getValue
  "Hello, " <> (toHtml txt) <> "!"

example :: [Iris] -> SHtml IO Example ()
example irisd = wrap plotlyCDN $ do
  nm  <- getValue
  let trace :: Trace
      trace = points (aes & x .~ (nm ^. xaxis . tagVal)
                          & y .~ (nm ^. yaxis . tagVal)) irisd
--                      & marker ?~ (defMarker & markercolor ?~ catColors (map irisClass irisd))

  h2_ "Testing Dashdo"
  
  select [("Male", True),("Female", False)] isMale
  br_ []

  "Name input #1:"
  textInput pname
  br_ []

  "Name input #2:"
  textInput pname
  br_ []

  "Name input #3:"
  textInput pname
  br_ []

  "Greetings using (#>):"
  pname #> hello
  br_ []
  
  isMale #> test
  br_ []
  
  select axes xaxis
  select axes yaxis
  toHtml  $ plotly "foo" [trace] & layout . title ?~  "my plot"

axes = [tagOpt "sepal length" sepalLength,
        tagOpt "sepal width" sepalWidth,
        tagOpt "petal length" petalLength,
        tagOpt "petal width" petalWidth]

initv = Example "Simon" True (snd $ axes!!0) (snd $ axes!!1)

{-hbarData :: [(Text, Double)]
hbarData = [("Simon", 14.5), ("Joe", 18.9), ("Dorothy", 16.2)]

hbarsTrace = bars & y ?~ map fst hbarData & x ?~ map snd hbarData & orientation ?~ Horizontal -}
