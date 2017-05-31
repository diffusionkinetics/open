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

