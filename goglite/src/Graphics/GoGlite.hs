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

aes = emptyBook

myPts :: [(Double, Double)]
myPts = [(1,2), (1.2, 3), (1.4,3.5)]

--type Person = Book '[ "name" :=> String , "age" :=> Int ]
--let julian :: Person = emptyBook & #age =: 28 & #name =: "Julian K. Arni"

--myTrace :: Plot.Trace
{-myTrace = points (aes & #x =: fst
                      & #y =: snd)
                 myPts -}
points :: forall book a b c.
          (Gettable "x" book (a->b),
           Gettable "y" book (a->c),
           ToJSON b,
           ToJSON c)
       => Book' book -> [a] -> Plot.Trace
points a xs =  Plot.scatter
                 & Plot.x ?~ map (toJSON @b . get #x a) xs
                 & Plot.y ?~ map (toJSON @c . get #y a) xs
                 & Plot.mode ?~ [Plot.Markers]
pts :: forall a book b. (Gettable "x" book (a->b), ToJSON b) => Book' book -> [a] -> [Value]
pts a xs = map (toJSON @b . get #x  a) xs

myvs = pts (aes & #x =: fst) myPts

--ptsVs a xs = map toJSON $ pts a xs
