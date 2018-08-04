{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{- |
Mushroom data set

See <https://archive.ics.uci.edu/ml/datasets/mushroom>

Data Set Information:

This data set includes descriptions of hypothetical samples corresponding to 23 species of gilled mushrooms in the Agaricus and Lepiota Family (pp. 500-525). Each species is identified as definitely edible, definitely poisonous, or of unknown edibility and not recommended. This latter class was combined with the poisonous one. The Guide clearly states that there is no simple rule for determining the edibility of a mushroom; no rule like ``leaflets three, let it be'' for Poisonous Oak and Ivy.

Attribute Information:

1. cap-shape: bell=b,conical=c,convex=x,flat=f, knobbed=k,sunken=s
2. cap-surface: fibrous=f,grooves=g,scaly=y,smooth=s
3. cap-color: brown=n,buff=b,cinnamon=c,gray=g,green=r, pink=p,purple=u,red=e,white=w,yellow=y
4. bruises?: bruises=t,no=f
5. odor: almond=a,anise=l,creosote=c,fishy=y,foul=f, musty=m,none=n,pungent=p,spicy=s
6. gill-attachment: attached=a,descending=d,free=f,notched=n
7. gill-spacing: close=c,crowded=w,distant=d
8. gill-size: broad=b,narrow=n
9. gill-color: black=k,brown=n,buff=b,chocolate=h,gray=g, green=r,orange=o,pink=p,purple=u,red=e, white=w,yellow=y
10. stalk-shape: enlarging=e,tapering=t
11. stalk-root: bulbous=b,club=c,cup=u,equal=e, rhizomorphs=z,rooted=r,missing=?
12. stalk-surface-above-ring: fibrous=f,scaly=y,silky=k,smooth=s
13. stalk-surface-below-ring: fibrous=f,scaly=y,silky=k,smooth=s
14. stalk-color-above-ring: brown=n,buff=b,cinnamon=c,gray=g,orange=o, pink=p,red=e,white=w,yellow=y
15. stalk-color-below-ring: brown=n,buff=b,cinnamon=c,gray=g,orange=o, pink=p,red=e,white=w,yellow=y
16. veil-type: partial=p,universal=u
17. veil-color: brown=n,orange=o,white=w,yellow=y
18. ring-number: none=n,one=o,two=t
19. ring-type: cobwebby=c,evanescent=e,flaring=f,large=l, none=n,pendant=p,sheathing=s,zone=z
20. spore-print-color: black=k,brown=n,buff=b,chocolate=h,green=r, orange=o,purple=u,white=w,yellow=y
21. population: abundant=a,clustered=c,numerous=n, scattered=s,several=v,solitary=y
22. habitat: grasses=g,leaves=l,meadows=m,paths=p, urban=u,waste=w,woods=d

Example rows:

p,x,s,n,t,p,f,c,n,k,e,e,s,s,w,w,p,w,o,p,k,s,u
e,x,s,y,t,a,f,c,b,k,e,c,s,s,w,w,p,w,o,p,n,n,g
-}
module Numeric.Datasets.Mushroom where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Control.Applicative

data Classification = Poisonous | Edible deriving (Eq, Show, Ord, Enum, Bounded, Generic)

data CapShape = Bell | Conical | Convex | Flat | Knobbed | Sunken deriving (Eq, Show, Ord, Enum, Bounded, Generic)

data CapSurface = Fibrous | Grooves | Scaly | Smooth deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- cap-color: brown=n,buff=b,cinnamon=c,gray=g,green=r, pink=p,purple=u,red=e,white=w,yellow=y
data CapColor = Brown | Buff | Cinnamon | Gray | Green | Pink | Purple | Red | White | Yellow deriving (Eq, Show, Ord, Enum, Bounded, Generic)

data Bruises = Bruises | NoBruises deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- odor: almond=a,anise=l,creosote=c,fishy=y,foul=f, musty=m,none=n,pungent=p,spicy=s
data Odor = Almond | Anise | Creosote | Fishy | Foul | Musty | None | Pungent | Spicy deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- gill-attachment: attached=a,descending=d,free=f,notched=n
data GillAttachment = Attached | Descending | Free | Notched deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- 7. gill-spacing: close=c,crowded=w,distant=d
data GillSpacing = Close | Crowded | Distant deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- 8. gill-size: broad=b,narrow=n
data GillSize = Broad | Narrow deriving (Eq, Show, Ord, Enum, Bounded, Generic)
-- 9. gill-color: black=k,brown=n,buff=b,chocolate=h,gray=g, green=r,orange=o,pink=p,purple=u,red=e, white=w,yellow=y
data GillColor = Black | Brown | Buff | Chocolate | Gray | Green | Orange | Pink | Purple | Red | White | Yellow deriving (Eq, Show, Ord, Enum, Bounded, Generic)
-- 10. stalk-shape: enlarging=e,tapering=t
data StalkShape = Enlarging | Tapering deriving (Eq, Show, Ord, Enum, Bounded, Generic)
-- 11. stalk-root: bulbous=b,club=c,cup=u,equal=e, rhizomorphs=z,rooted=r,missing=?
data StalkRoot = Bulbous | Club | Cup | Equal | Rhizomorphs | Rooted | Missing deriving (Eq, Show, Ord, Enum, Bounded, Generic)
-- 12. stalk-surface-above-ring: fibrous=f,scaly=y,silky=k,smooth=s
data StalkSurfaceAboveRing = Fibrous | Scaly | Silky | Smooth deriving (Eq, Show, Ord, Enum, Bounded, Generic)
-- 13. stalk-surface-below-ring: fibrous=f,scaly=y,silky=k,smooth=s
data StalkSurfaceBelowRing = Fibrous | Scaly | Silky | Smooth deriving (Eq, Show, Ord, Enum, Bounded, Generic)
-- 14. stalk-color-above-ring: brown=n,buff=b,cinnamon=c,gray=g,orange=o, pink=p,red=e,white=w,yellow=y
data StalkColorAboveRing = Brown | Buff | Cinnamon | Gray | Orange | Pink | Red | White | Yellow deriving (Eq, Show, Ord, Enum, Bounded, Generic)
-- 15. stalk-color-below-ring: brown=n,buff=b,cinnamon=c,gray=g,orange=o, pink=p,red=e,white=w,yellow=y
data StalkColorBelowRing = Brown | Buff | Cinnamon | Gray | Orange | Pink | Red | White | Yellow deriving (Eq, Show, Ord, Enum, Bounded, Generic)
-- 16. veil-type: partial=p,universal=u
data VeilType = Partial | Universal deriving (Eq, Show, Ord, Enum, Bounded, Generic)
-- 17. veil-color: brown=n,orange=o,white=w,yellow=y
data VeilColor = Brown | Orange | White | Yellow deriving (Eq, Show, Ord, Enum, Bounded, Generic)
-- 18. ring-number: none=n,one=o,two=t
data RingNumber = None | One | Two deriving (Eq, Show, Ord, Enum, Bounded, Generic)
-- 19. ring-type: cobwebby=c,evanescent=e,flaring=f,large=l, none=n,pendant=p,sheathing=s,zone=z
data RingType = Cobwebby | Evanescent | Flaring | Large | None | Pendant | Sheathing | Zone deriving (Eq, Show, Ord, Enum, Bounded, Generic)
-- 20. spore-print-color: black=k,brown=n,buff=b,chocolate=h,green=r, orange=o,purple=u,white=w,yellow=y
data SporePrintColor = Black | Brown | Buff | Chocolate | Green | Orange | Purple | White | Yellow deriving (Eq, Show, Ord, Enum, Bounded, Generic)
-- 21. population: abundant=a,clustered=c,numerous=n, scattered=s,several=v,solitary=y
data Population = Abundant | Clustered | Numerous | Scattered | Several | Solitary deriving (Eq, Show, Ord, Enum, Bounded, Generic)
-- 22. habitat: grasses=g,leaves=l,meadows=m,paths=p, urban=u,waste=w,woods=d
data Habitat = Grasses | Leaves | Meadows | Paths | Urban | Waste | Woods deriving (Eq, Show, Ord, Enum, Bounded, Generic)
