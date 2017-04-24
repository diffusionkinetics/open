{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ExtendedDefaultRules, FlexibleContexts, TemplateHaskell #-}

import Numeric.Datasets.Iris

import Dashdo
import Dashdo.Types
import Dashdo.Serve
import Dashdo.Elements
import Control.Monad
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
 , _xaxis :: Tag (Iris -> Double)
 , _yaxis :: Tag (Iris -> Double)
 }

makeLenses ''IKM

axes = [tagOpt "sepal length" sepalLength,
        tagOpt "sepal width" sepalWidth,
        tagOpt "petal length" petalLength,
        tagOpt "petal width" petalWidth]

ikm0 = IKM 3 (snd $ axes!!0) (snd $ axes!!1)

irisData 
  = (const () 
    ~~ [sepalLength, sepalWidth, petalLength, petalWidth])
    iris

main = runDashdo $ pureDashdo ikm0 dashdo

dashdo ikm = wrap plotlyCDN $ do
    let ctrs :: [VS.Vector Double]
        ctrs = model $ runIdentity $ runSupervisor (kmeans $ ikm ^. nclusters) Nothing irisData
    h2_ "Iris k-means clustering"    
    row_ $ do
        mkCol [(MD,3)] $ do
            numInput (Just 0) Nothing (Just 1) nclusters
            select axes xaxis
            select axes yaxis
        mkCol [(MD,9)] $ do
            let trace :: Trace
                trace = points (aes & x .~ (ikm ^. xaxis . tagVal)
                                    & y .~ (ikm ^. yaxis . tagVal)) 
                                iris
            toHtml $ plotly "foo" [trace] & layout . title ?~  "my plot"
        
    return ()