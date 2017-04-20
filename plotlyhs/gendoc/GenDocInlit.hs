{-# LANGUAGE OverloadedStrings,TemplateHaskell, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -F -pgmF inlitpp #-}

# Plotly.js Haskell bindings examples

```haskell hide top
import Inliterate.Import
import Graphics.Plotly hiding (text)
import Graphics.Plotly.Lucid
import Data.Text (Text)
import Lens.Micro
import Numeric.Datasets.Iris
import Data.Aeson
```

```html_header
<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
```

This web pages shows plots generated with the plotlyhs packages, along with the
Haskell code that generated the plots. To use the plots on the generate page,
the Plotly.js source code should first be including by adding this tag to your HTML
header:

```html
<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
```

Alternatively, this tag can be included in an HTML page using the `plotlyCDN`
function in `Graphics.Plotly.Lucid` (when using Lucid) or
`Graphics.Plotly.Blaze`(when using blaze-html).

### A complete & minimal example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Lucid
import Lucid.Html5
import Graphics.Plotly
import Graphics.Plotly.Lucid
import Lens.Micro

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

main =
    T.writeFile "test.html" $ renderText $ doctypehtml_ $ do
    head_ $ do meta_ [charset_ "utf-8"]
               plotlyCDN
    body_ $ toHtml $ plotly "myDiv" [myTrace]

pointsData :: [(Double, Double)]
pointsData = zip [1,2,3,4] [500,3000,700,200]

myTrace
  = line (aes & x .~ fst
              & y .~ snd) pointsData
```


```haskell eval
plotly "p0" [line (aes & x .~ fst & y .~ snd) pointsData]
```

In the examples below, we omit all of the imports, 
main function, html header and focus only
on the `Plotly` value (the argument to `toHtml`). The
`Plotly` value can be constructed with the function `plotly`
which takes two arguments: the element id of the `<div>`
for the plot (this element will be created if you call toHtml on the `Plotly`
value) and a list of traces.

### A simple plot

We use these two basic datasets:

```haskell top
hbarData :: [(Text, Double)]
hbarData = [("Simon", 14.5), ("Joe", 18.9), ("Dorothy", 16.2)]

pointsData :: [(Double, Double)]
pointsData = zip [1,2,3,4] [500,3000,700,200]
```

Code to generate build the `Plotly` value 
(which has a `toHtml` instance), result on the right

```haskell eval twocol
plotly "div1" [line (aes & x .~ fst 
                         & y .~ snd) 
                    pointsData]
```

The above is quite unbearably sized & padded for this tutorial, so let's fix the 
margins and the plot height

```haskell eval twocol
plotly "div2" [line (aes & x .~ fst 
                         & y .~ snd) 
                    pointsData]
    & layout . margin ?~ thinMargins
    & layout . height ?~ 300
```

### Lines and Markers

```haskell eval twocol
plotly "div3" [points (aes & x .~ fst 
                           & y .~ snd) 
                      pointsData]
    & layout . margin ?~ thinMargins
    & layout . height ?~ 300
```

```haskell eval twocol
plotly "div4" [points (aes & x .~ fst 
                           & y .~ snd)
                      pointsData 
                  & mode ?~ [Lines, Markers]]
    & layout . margin ?~ thinMargins
    & layout . height ?~ 300
```

### Iris plots

This plot uses the iris value from the datasets package

```haskell eval twocol
plotly "div6"
    [points (aes & x .~ sepalLength 
                 & y .~ sepalWidth
                 & color ?~ (fromEnum . irisClass)) iris]
    & layout . margin ?~ thinMargins
    & layout . height ?~ 300    
```

### Horizontal bar plots