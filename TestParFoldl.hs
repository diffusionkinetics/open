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
                   <*> pure Setosa

deriving instance Ord IrisClass

main = do print $ ("iris average seplen", fold (premap sepalLength average) iris)
          print $ ("iris variance seplen", fold (premap sepalLength variance) iris)
          print $ fold (irisApply average) iris
          let byClass = map updClass $ Map.toList $ fold (groupBy irisClass $ irisApply average) iris
          mapM_ print byClass
          bh <- getDataset bostonHousing
          print $ length bh
          print $ fold (premap tax average) bh
          print $ fold (premap tax variance) bh

updClass :: (IrisClass, Iris) -> Iris
updClass (c, i) = i { irisClass = c }
