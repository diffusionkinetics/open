datasets: data sets for statistics and machine learning, in Haskell
=====

This library provides easy access in Haskell to a series of data sets
for Statistics and Machine learning.

## Usage

```haskell

import Numeric.Datasets (getDataset)
import Numeric.Datasets.Iris (iris)

main = do
  irises <- getDataset iris
  print (length irises)
  print (head irises)

```
