Plotlyhs: Haskell bindings for Plotly.js
=====

[![Hackage](https://img.shields.io/hackage/v/plotlyhs.svg)](https://hackage.haskell.org/package/plotlyhs) [![Build Status](https://secure.travis-ci.org/glutamate/plotlyhs.svg)](http://travis-ci.org/glutamate/plotlyhs)

This is a library for generating JSON value to use with the Plotly.js
library. The interface directly reflects the structure of the
Plotly.js library and is therefore quite low-level. Lenses are used
throughout to set `Maybe` fields in records to provide both data and configuration options.

This library does *not* attempt to communicate with the Plotly API in
any other way. All generated plots can be hosted on stand-alone web
pages.

## Example

For more, [see the examples page](https://glutamate.github.io/plotlyhs/)

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

myTrace = scatter & x ?~ [1,2,3,4]
                  & y ?~ [500,3000,700,200]
```
