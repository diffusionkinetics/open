{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, TemplateHaskell #-}
module Dashdo.Examples.IrisKMeans where

import Numeric.Datasets.Iris

import Dashdo
import Dashdo.Types
import Dashdo.Serve
import Dashdo.Elements hiding (numInput)
import Dashdo.FlexibleInput hiding (textInput)
import Control.Monad
import Control.Monad.State.Strict
import Lucid
import Lucid.Bootstrap3
import Lucid.Bootstrap
import Data.Monoid ((<>))
import Data.Text (Text, unpack, pack)
import Lens.Micro.Platform
import Fuml.Core
import Fuml.Unsupervised
import qualified Data.Vector.Storable as VS

import Graphics.Plotly hiding (xaxis, yaxis)
import Graphics.Plotly.Lucid
import Control.Monad.Identity

data IKM = IKM
 { _nclusters :: Int
 , _xaxis :: Tag (Int, Iris -> Double)
 , _yaxis :: Tag (Int, Iris -> Double)
 }

makeLenses ''IKM

axes = [tagOpt "sepal length" (0, sepalLength),
        tagOpt "sepal width" (1, sepalWidth),
        tagOpt "petal length" (2, petalLength),
        tagOpt "petal width" (3, petalWidth)]

ikm0 = IKM 3 (snd $ axes!!0) (snd $ axes!!1)

irisData
  = (const ()
    ~~ [sepalLength, sepalWidth, petalLength, petalWidth])
    iris

irisKMeans = runDashdoIO $ Dashdo ikm0 dashdo

dashdo :: SHtml IO IKM ()
dashdo = wrap plotlyCDN $ do
    ikm <- getValue
    let ctrs :: [VS.Vector Double]
        ctrs = model $ runIdentity $ runSupervisor (kmeans $ ikm ^. nclusters) Nothing irisData

    h2_ "Iris k-means clustering"
    row_ $ do
        mkCol [(MD,3)] $ div_ [class_ "well"] $ do
            "X Variable"
            br_ []
            xaxis <<~ select axes
            br_ []
            "y Variable"
            br_ []
            yaxis <<~ select axes
            br_ []
            "Cluster count"
            br_ []
            nclusters <<~ numInput  & minVal ?~ 0 & step ?~ 1

        mkCol [(MD,9)] $ do
            let trace :: Trace
                trace = points (aes & x .~ (ikm ^. xaxis . tagVal . _2)
                                    & y .~ (ikm ^. yaxis . tagVal . _2))
                                iris
                traceCtrs
                   = points (aes & x .~ (VS.! (ikm ^. xaxis . tagVal . _1))
                                 & y .~ (VS.! (ikm ^. yaxis . tagVal . _1)))
                                ctrs
            toHtml $ plotly "foo" [trace, traceCtrs]
                       & layout . margin ?~ thinMargins