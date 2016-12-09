{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeFamilies, FlexibleContexts #-}

{-|
A limited Grammar of Graphics-like interface.
-}

module Graphics.Plotly.GoG where

import qualified Graphics.Plotly as Plot
import Data.Text (Text)
import Lens.Micro
import Data.Aeson

class ToJSON a => AxisValue a

instance AxisValue Double
instance AxisValue Float
instance AxisValue Text
instance AxisValue String
instance AxisValue Int

type family XVal a
type family YVal a

type instance XVal (a,b) = a
type instance YVal (a,b) = b

data Aes t a = Aes { _x :: (a -> XVal t)
                   , _y :: (a -> YVal t)
                   , _color :: Maybe (a -> Int)
                   , _size :: Maybe (a -> Double)
                   }
aes :: Aes ((), ()) a
aes = Aes (const ()) (const ()) Nothing Nothing

setx :: AxisValue v => Aes (vx,vy) a -> (a -> v) -> Aes (v, vy) a
setx (Aes _ fy fc fs) f = (Aes f fy fc fs) --a { _x = f}

x :: AxisValue v => Lens (Aes (vx,vy) a) (Aes (v,vy) a) (a -> vx) (a -> v)
x = lens _x setx

sety :: AxisValue v => Aes (vx,vy) a -> (a -> v) -> Aes (vx, v) a
sety (Aes fx _ fc fs) f = (Aes fx f fc fs) --a { _x = f}
--sety a f = a { _y = f}

y :: AxisValue v => Lens (Aes (vx,vy) a) (Aes (vx,v) a) (a -> vy) (a -> v)
y = lens _y sety


points :: (AxisValue (XVal t), AxisValue (YVal t), Num (XVal t), Num (YVal t))
       => Aes t a -> [a] -> Plot.Trace
points a xs = Plot.scatter & Plot.x ?~ map (toJSON . _x a) xs
                 & Plot.y ?~ map (toJSON . _y a) xs
                 & Plot.mode ?~ [Plot.Markers]

line :: (AxisValue (XVal t), AxisValue (YVal t), Num (XVal t), Num (YVal t))
       => Aes t a -> [a] -> Plot.Trace
line a xs = Plot.scatter & Plot.x ?~ map (toJSON . _x a) xs
                 & Plot.y ?~ map (toJSON . _y a) xs
                 & Plot.mode ?~ [Plot.Lines]

hbars :: (AxisValue (XVal t), AxisValue (YVal t), Num (XVal t))
       => Aes t a -> [a] -> Plot.Trace
hbars a xs = Plot.bars & Plot.x ?~ map (toJSON . _x a) xs
                 & Plot.y ?~ map (toJSON . _y a) xs
                 & Plot.orientation ?~ Plot.Horizontal


myPts :: [(Double, Double)]
myPts = [(1,2), (1.2, 3), (1.4,3.5)]

myTrace :: Plot.Trace
myTrace = points (aes & x .~ fst
                      & y .~ snd)
                 myPts
