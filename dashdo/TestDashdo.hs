{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, TemplateHaskell #-}

import Numeric.Datasets.Iris

import Dashdo
import Dashdo.Types
import Dashdo.Serve
import Dashdo.Elements
import Control.Monad
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


main = do
  runDashdo theDashdo

theDashdo = Dashdo initv getDraws (example iris)

getDraws :: Example -> IO [Double]
getDraws nm = do
  sample $ replicateM 1000 $ normal 0 1

example :: [Iris] -> Example -> [Double] -> SHtml Example ()
example irisd nm xs = wrap plotlyCDN $ do
  let ptitle = if _isMale nm then "Mr " else "Ms "
      trace :: Trace
      trace = points (aes & x .~ (nm ^. xaxis . tagVal)
                          & y .~ (nm ^. yaxis . tagVal)) irisd
--                      & marker ?~ (defMarker & markercolor ?~ catColors (map irisClass irisd))

  h2_ "Testing Dashdo"
  textInput pname
  select [("Male", True),("Female", False)] isMale
  br_ []
  "Hello "<> ptitle <> (toHtml $ nm ^. pname)
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
