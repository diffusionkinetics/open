inliterate: Dynamic reporting for Haskell
=======

The aim of inliterate is generate dynamic markdown reports from literate Haskell code in which
code blocks can be evaluated to present the results of analyses in textual or graphical form.

inliterate is a GHC preprocessor which transforms a markdown document into a Haskell program, 
which, when run, prints to stdout the input document in HTML format. Certain code blocks with special
annotations can be treated in particular ways: as Haskell code that must be included in the 
generating program (at the top level or in a do block) and as code that must be evaluated with the results
inserted into the HTML document.

For an example document, see https://github.com/filopodia/open/blob/master/plotlyhs/gendoc/GenDocInlit.hs which 
ccompiles into https://glutamate.github.io/plotlyhs/.

## The inliterate document

The inliterate ddocument should contain the following pragma on the first line or after the LANGUAGE pragmas:

```
{-# OPTIONS_GHC -F -pgmF inlitpp #-}
```

this tells GHC to invoke the preprocessor.

### Code blocks

inliterate gives special meaning to the following code block language annotations (see the plotlyhs documentation 
generator to see how these are invoked)

#### `html_header`

this HTML should go in the header. Use this to load JavaScript or CSS, for instance for graphing libraries.

#### `haskell top`

Haskell code that is added to the top level

#### `haskell do`

Haskell code that is added to the main do block. use this for loading data or otherwise performing input and output.

#### `haskell eval`

Haskell code that is evaluated. The resultant type must be an instance of the `AskInliterate` class ddefined in 
Inliterate.Import. 

#### `hide`

add this to `top` or `do` code to prevent it from being printed in the output document (but it is still run)

## How to run

### As an excutable

if you only have a few documents, you can put them in your cabal file as an executable, with `inliterate` as a build dependency.

here's the example from the plotlyhs [documentation generator](https://github.com/filopodia/open/blob/bf3e3211f936d1c66ee5f4828a6e26d4a2d5df76/plotlyhs/gendoc/plotly-gendoc.cabal#L15-L26):

```
executable plotly-gendoc
  main-is: GenDocInlit.hs
  build-depends:       base >=4.6 && <5
                     , plotlyhs
                     , lucid
                     , aeson
                     , text
                     , microlens
                     , plotlyhs
                     , inliterate
                     , datasets
                     , neat-interpolation
```

### Using stack runghc

if you have a larger number, or dynamically changing, set of documents, you can run them individually using stack runghc. `inliterate`
should be listed in your cabal file as a build dependency to make sure the package is visible.

```
stack runghc InliterateFile.hs
```
if you want only the body HTML and not the headers, you can pass the argument `--no-inlit-wrap` to the executable
```
stack runghc InliterateFile.hs -- --no-inlit-wrap
```