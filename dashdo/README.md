## Dashdo

A framework for writing web-based analytics dashboards in Haskell, inspired by [Shiny](https://shiny.rstudio.com/).

Dashdo dashboards are defined over a Haskell type which specify the state of the dashboard. Form elements are tied to
lenses into the state value, and the displayed content is calculated from the state. Dashboards are defined in
as single record type that can be interpreted in multiple different ways.

```haskell
{-
            +------------------- the state of the dashboard
            |
            |          +-------- the state is augmented
            |          |          with information fetched in `IO`
            |          |
            v          v             -}
data Dashdo t = forall b. Dashdo
  { initial :: t                    -- the initial state
  , fetch :: t -> IO b              -- fetch more data
  , render :: t -> b -> SHtml t ()  -- render the dashboard
  }  {-                  /|\
                          |
                          +-------- generate HTML, and keep track
                                      of how form elements bind
                                      lenses into state
     -}
```

`SHtml` is a Lucid monad transformer and you can use all the usual Lucid combinators.

Run your dashdo dashboard with Dashdo.Serve.runDashdo.

### Examples

For a simple example, we will just ask for the users name. We need a simple state containing that name. We could just 
use a `Text` value as the state and the identity lens, but a record is a more realistic starting point for when you
want to add functionality.

```haskell
data Example = Example
 { _name :: Text
 }

makeLenses ''Example
```

We then need to show the dashboard as a function of the state, together with the form element (here an `<input>`) 
to change the state.

```haskell
myRender :: Example -> SHtml Example ()
myRender s = do
  h2_ "Testing Dashdo"
  textInput name
  br_ []
  "Hello " <> (toHtml $ s ^. name)
```

Since we are not making use of the functionality to fetch extra information in the `IO` monad, we can use
the `pureDashdo` function to generate our `Dashdo`.

```haskell
main = runDashdo $ pureDashdo "George" myRender
```

For more examples, see the [dashdo-examples](https://github.com/diffusionkinetics/open/tree/master/dashdo-examples) directory.

### To re-run on file changes

`git ls-files | entr -r stack build --exec test-dashdo`

or

`find . -name '*.hs' ! -path '*stack-work*' | entr -r stack build --exec test-dashdo`