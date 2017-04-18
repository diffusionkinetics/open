{-# LANGUAGE StandaloneDeriving     #-}

import Control.Parallel.Foldl
import Numeric.Datasets.Iris
import Numeric.Datasets.BostonHousing
import Numeric.Datasets
import qualified Data.Map.Strict as Map

irisApply :: Fold Double Double -> Fold Iris Iris
irisApply f = Iris <$> premap sepalLength f
                   <*> premap sepalWidth f
                   <*> premap petalLength f
                   <*> premap petalWidth f
                   <*> premap irisClass mode

main :: IO ()
main = do print $ ("iris average seplen", fold (premap sepalLength average) iris)
          print $ ("iris variance seplen", fold (premap sepalLength variance) iris)
          print $ ("iris twopvar  seplen", twoPassVariance $ map sepalLength iris)
          print $ fold (irisApply average) iris
          let byClass = Map.toList $ fold (groupBy irisClass $ irisApply average) iris
          mapM_ print byClass
          bh <- getDataset bostonHousing
          print $ length bh
          print $ fold (premap tax average) bh
          print $ fold (premap tax variance) bh
          print $ twoPassVariance $ map tax bh
          let manyNums = [1..1000000]
          print $ twoPassVariance manyNums
          print $ fold variance manyNums


{-

scikit-learn

>>> iris.data[:,0].var(ddof=0)
0.6811222222222223
>>> iris.data[:,0].var(ddof=1)
0.68569351230425069

>>> boston.data[:,9].var(ddof=0)
28348.62359980628
>>> boston.data[:,9].var(ddof=1)
28404.759488122731

-}
