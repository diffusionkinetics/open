## Shany

A Shiny clone for Haskell

### To re-run on file changes

`git ls-files | entr -r stack build --exec test-shany`

or

`find . -name '*.hs' ! -path '*stack-work*' | entr -r stack build --exec test-shany`