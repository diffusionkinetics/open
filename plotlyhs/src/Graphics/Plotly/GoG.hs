{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeApplications   #-}

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
module Graphics.Plotly.GoG where

import           Data.Aeson
import           Data.Text (Text)
import           Data.Time
import           Lens.Micro

import qualified Graphics.Plotly.Base as Plot


class ToJSON a => AxisValue a

instance AxisValue Double
instance AxisValue Float
instance AxisValue Text
instance AxisValue String
instance AxisValue Int
instance AxisValue Day

data RGB a = RGB a a a
data RGBA a = RGBA a a a a

instance ToJSON (RGB Int) where
  toJSON (RGB r g b) = toJSON $ concat ["rgb(",show r,",",show g, ",", show b,")"]

instance ToJSON (RGB Double) where
  toJSON (RGB r g b) = toJSON $ concat ["rgb(",showd r,",",showd g, ",", showd b,")"]
   where showd = show @Int. floor . (*256)

instance ToJSON (RGBA Int) where
  toJSON (RGBA r g b a) = toJSON $ concat ["rgba(",show r,",",show g, ",", show b,",", show a, ")"]

instance ToJSON (RGBA Double) where
  toJSON (RGBA r g b a) = toJSON $ concat ["rgb(",showd r,",",showd g, ",", showd b,",", showd a,")"]
   where showd = show @Int. floor . (*256)

class ToJSON a => IsColor a

instance IsColor Int
instance IsColor (RGB Int)
instance IsColor (RGB Double)
instance IsColor (RGBA Int)
instance IsColor (RGBA Double)

type family XVal a
type family YVal a
type family ZVal a
type family CVal a
type family SVal a

type instance XVal (x,y,z,c,s) = x
type instance YVal (x,y,z,c,s) = y
type instance ZVal (x,y,z,c,s) = z
type instance CVal (x,y,z,c,s) = c
type instance SVal (x,y,z,c,s) = s

data Aes t a = Aes
    { _x     :: a -> XVal t
    , _y     :: a -> YVal t
    , _z     :: a -> ZVal t
    , _color :: Maybe (a -> CVal t)
    , _size  :: Maybe (a -> SVal t)
    }


aes :: Aes ((), (), (), (), ()) a
aes = Aes (const ()) (const ()) (const ()) Nothing Nothing


setx :: (AxisValue v)
    => Aes (vx,vy,vz,vc,vs) a -> (a -> v) -> Aes (v, vy, vz, vc, vs) a
setx (Aes _ fy fz fc fs) f = Aes f fy fz fc fs


x :: (AxisValue v)
    => Lens (Aes (vx,vy, vz, vc, vs) a) (Aes (v,vy, vz, vc, vs) a) (a -> vx) (a -> v)
x = lens _x setx


sety :: (AxisValue v)
    => Aes (vx,vy, vz, vc, vs) a -> (a -> v) -> Aes (vx, v, vz, vc, vs) a
sety (Aes fx _ fz fc fs) f = Aes fx f fz fc fs


y :: (AxisValue v)
    => Lens (Aes (vx,vy, vz, vc, vs) a) (Aes (vx,v, vz, vc, vs) a) (a -> vy) (a -> v)
y = lens _y sety

setz :: (AxisValue v)
    => Aes (vx,vy, vz, vc, vs) a -> (a -> v) -> Aes (vx, vy, v, vc, vs) a
setz (Aes fx fy _ fc fs) f = Aes fx fy f fc fs


z :: (AxisValue v)
    => Lens (Aes (vx,vy, vz, vc, vs) a) (Aes (vx,vy, v, vc, vs) a) (a -> vz) (a -> v)
z = lens _z setz

setcol :: (IsColor v)
    => Aes (vx,vy, vz, vc, vs) a -> Maybe (a -> v) -> Aes (vx, vy, vz, v, vs) a
setcol (Aes fx fy fz _ fs) f = Aes fx fy fz f fs


color :: (IsColor v)
    => Lens (Aes (vx,vy, vz, vc, vs) a) (Aes (vx,vy,vz, v,vs) a) (Maybe (a -> vc)) (Maybe (a -> v))
color = lens _color setcol


setsize :: (AxisValue v, Num v)
    => Aes (vx,vy, vz, vc, vs) a -> Maybe (a -> v) -> Aes (vx, vy, vz, vc, v) a
setsize (Aes fx fy fz fc _) = Aes fx fy fz fc


size :: (AxisValue v, Num v)
    => Lens (Aes (vx,vy, vz, vc, vs) a) (Aes (vx,vy,vz, vc,v) a) (Maybe (a -> vs)) (Maybe (a -> v))
size = lens _size setsize


points :: (AxisValue (XVal t), AxisValue (YVal t), ToJSON (CVal t), ToJSON (SVal t))
       => Aes t a -> [a] -> Plot.Trace
points a xs =  setSize (_size a) $ setColors (_color a) $ Plot.scatter
                 & Plot.x ?~ map (toJSON . _x a) xs
                 & Plot.y ?~ map (toJSON . _y a) xs
                 & Plot.mode ?~ [Plot.Markers]
  where setColors Nothing p = p
        setColors (Just setC) p
          = p & Plot.marker . non Plot.defMarker . Plot.markercolor ?~ Plot.List (map (toJSON . setC) xs)
        setSize Nothing p = p
        setSize (Just setS) p
          = p & Plot.marker . non Plot.defMarker . Plot.size ?~ Plot.List (map (toJSON . setS) xs)

line :: (AxisValue (XVal t), AxisValue (YVal t))
       => Aes t a -> [a] -> Plot.Trace
line a xs = Plot.scatter & Plot.x ?~ map (toJSON . _x a) xs
                 & Plot.y ?~ map (toJSON . _y a) xs
                 & Plot.mode ?~ [Plot.Lines]

hbars :: (AxisValue (XVal t), AxisValue (YVal t))
       => Aes t a -> [a] -> Plot.Trace
hbars a xs = Plot.bars & Plot.x ?~ map (toJSON . _x a) xs
                 & Plot.y ?~ map (toJSON . _y a) xs
                 & Plot.orientation ?~ Plot.Horizontal

hcontour :: (AxisValue (XVal t), AxisValue (YVal t), AxisValue (ZVal t))
         => Aes t a -> [a] -> Plot.Trace
hcontour a xs = Plot.contour
                & Plot.x ?~ map (toJSON . _x a) xs
                & Plot.y ?~ map (toJSON . _y a) xs
                & Plot.z ?~ map (toJSON . _z a) xs
                & Plot.orientation ?~ Plot.Horizontal
