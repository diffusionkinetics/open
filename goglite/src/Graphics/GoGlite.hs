{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeFamilies, FlexibleContexts,
              OverloadedLabels, TypeOperators, ScopedTypeVariables, DataKinds,
              TypeApplications, AllowAmbiguousTypes#-}

module Graphics.GoGlite where


{-|
A limited Grammar of Graphics-like interface.

@
myPts :: [(Double, Double)]
myPts = [(1,2), (1.2, 3), (1.4,3.5)]



myTrace :: Trace
myTrace = points (aes & x .~ fst
                      & y .~ snd)
                 myPts
@



-}

import qualified Graphics.Plotly.Base as Plot
import Graphics.Plotly.GoG (AxisValue, IsColor, RGB (..),RGBA (..))
import Data.Text (Text)
import Lens.Micro
import Data.Aeson
import Bookkeeper
import Bookkeeper.Internal

aes ::  Book '[ "size" :=> Maybe (a->b) ]
aes = emptyBook & #size =: Nothing

myPts :: [(Double, Double)]
myPts = [(1,2), (1.2, 3), (1.4,3.5)]

myTrace :: Plot.Trace
myTrace = points (aes & #x =: fst
                      & #y =: snd
                      & #size =: Just fst)
                 myPts
points :: forall book a b c sz.
          (Gettable "x" book (a->b),
           Gettable "y" book (a->c),
           Gettable "size" book (Maybe (a->sz)),
           ToJSON b,
           ToJSON c,
           ToJSON sz,
           Num b,
           Num c)
       => Book' book -> [a] -> Plot.Trace
points a xs =  setSize (get #size a) $ Plot.scatter
                 & Plot.x ?~ map (toJSON @b . get #x a) xs
                 & Plot.y ?~ map (toJSON @c . get #y a) xs
                 & Plot.mode ?~ [Plot.Markers]
  where setSize Nothing p = p
        setSize (Just setS) p
          = p & Plot.marker . non Plot.defMarker . Plot.size ?~ Plot.List (map (toJSON @sz. setS) xs)

hbars :: forall book a b c.
          (Gettable "x" book (a->b),
           Gettable "y" book (a->c),
           ToJSON b,
           ToJSON c,
           Num b,
           AxisValue c)
       => Book' book -> [a] -> Plot.Trace
hbars a xs = Plot.bars & Plot.x ?~ map (toJSON @b. get #x a) xs
                 & Plot.y ?~ map (toJSON @c. get #y a) xs
                 & Plot.orientation ?~ Plot.Horizontal

{-
hbarSelect :: forall book a b c.
          (Gettable "x" book (a->b),
           Gettable "y" book (a->c),
           Gettable "select" book (a->s),
           ToJSON b,
           ToJSON c,
           ToJSON s,
           Num c,
           AxisValue b)
       => Book' book -> [a] -> Trace
hbarSelect a xs l = ?????

usage:

data PTable = PTable { _blessedPerson :: Maybe String } -- our form control

data Person = Person { name:: String, age :: Int }

persons :: [Person]

hbarSelect (aes & #x =: age
                & #y =: name
                & #select =: name) persons

plotlySelect :: FromJSON b => Plotly -> Lens a b -> SHtml a ()



ALTERNATIVE (BETTER)
====================

this version always selects the entire record - not sure how feasible it
is with the js/forms?

hbarSelect :: forall book a b c.
          (Gettable "x" book (a->b),
           Gettable "y" book (a->c),
           ToJSON b,
           ToJSON c,
           Num c,
           AxisValue b)
       => Book' book -> [a] -> Lens' t (Maybe a) -> SHtml t ()
hbarSelect a xs l = ?????

usage:

data PTable = PTable { _blessedPerson :: Maybe Person } -- our form control

data Person = Person { name:: String, age :: Int }

persons :: [Person]

hbarSelect (aes & #x =: age
                & #y =: name) persons blessedPerson




-}