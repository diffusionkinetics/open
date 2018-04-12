module Stan.Data where

import Data.List
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.Random.Source.PureMT

class Dump1 a where
  dump1 :: a -> ([String], [Int])

instance Dump1 Double where
    dump1 x = ([show x], [])
instance Dump1 Int where
    dump1 x = ([show x], [])
instance Dump1 Bool where
    dump1 True = (["1"], [])
    dump1 False = (["0"], [])

--dump1c xs = concat $ "c(" : intersperse "," (map dump1 xs) ++[")"]

instance Dump1 a => Dump1 [a] where
    dump1 xs = let xss = map dump1 xs
               in (concat $ transpose $ map fst xss, length xss:(snd $ head xss) )

instance Dump1 a => Dump1 (V.Vector a) where
    dump1 xs = dump1 $ V.toList xs

dumpAll :: ([String], [Int]) -> String
dumpAll ([s], []) = s
dumpAll (ss, [_]) = concat $ "c(" : intersperse "," ss ++[")"]
dumpAll (ss, ns) =
    concat $ "structure(c(" : intersperse "," ss ++["), .Dim = c("]
                             ++intersperse "," (map show ns)++["))"]

dumpAs :: ToStanData a => String -> a -> StanData
dumpAs nm x = Map.singleton nm $ toStanData x

(<~) :: ToStanData a => String -> a -> StanData
(<~) = dumpAs

type StanData = StanEnv

inGroupsOf :: Int -> [a] -> [[a]]
inGroupsOf _ [] = []
inGroupsOf n xs = let (a,b) = splitAt n xs in a : inGroupsOf n b

data StanValue = VDouble Double
               | VInt Int
               | VArray (V.Vector StanValue)
               | VSamples (V.Vector StanValue)
               | VSeed PureMT
               deriving (Show)

instance Dump1 StanValue where
    dump1 (VDouble x) = dump1 x
    dump1 (VInt x) = dump1 x
    dump1 (VArray x) = dump1 x
    dump1 (VSamples _) = error "Dump1 VSamples ¯\\_(ツ)_/¯"

type StanEnv = Map.Map String StanValue

dumpEnv :: StanEnv -> [String]
dumpEnv = map f . Map.toList where
    f (nm,sv) = nm++"<-"++(dumpAll $ dump1 sv)

class ToStanData a where
    toStanData :: a -> StanValue

instance ToStanData Double where
    toStanData x = VDouble x
instance ToStanData Int where
    toStanData x = VInt x
instance ToStanData Bool where
    toStanData True = VInt 1
    toStanData False = VInt 0
instance ToStanData a => ToStanData [a] where
    toStanData xs = VArray $ V.fromList $ map toStanData xs
instance ToStanData a => ToStanData (V.Vector a) where
    toStanData xs = VArray $ V.map toStanData xs

