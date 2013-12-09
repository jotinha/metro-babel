module Forcefield(
  enerTotal,
  enerOnePos,
  doubleLoop',
) where

import Utils

minimg_r2 p0 p1 box = normsq ( zipWith3 minimgx p0 p1 box )
  where minimgx x0 x1 lx = dx - lx * (fromIntegral (round (dx/lx))) where dx = x1 - x0

minimg_dr p0 p1 box = sqrt (minimg_r2 p0 p1 box)
      
lenjones_dr r = 4*(_r^12 - _r^6) where _r = 1.0/r
lenjones_r2 0.0 = error "lenjones got zero distance"
lenjones_r2 r2 = 4*(_r2^6 - _r2^3) where _r2 = 1.0/r2

-- doubleLoop funacc funpair list
-- Loop over all pairs of elements of input list without repeatition
--    funacc: accumulator function to apply to each successive result from all pairs
--    funpair: function to apply to each pair of elements
--    list: list of elements
doubleLoop :: (Num b) => (b -> b -> b) -> (a -> a -> b) -> [a] -> b
doubleLoop funacc funpair [] = 0
doubleLoop funacc funpair (x1:[]) = error "list must have at least 2 elements"
doubleLoop funacc funpair (x1:x2:[]) = funpair x1 x2
doubleLoop funacc funpair (x:xs) = funacc (singleLoop funacc (funpair x) xs) (doubleLoop funacc funpair xs)

-- this version does not accumulate the result and instead returns a list
doubleLoop' :: (a -> a -> b) -> [a] -> [b]
doubleLoop' funpair [] = []
doubleLoop' funpair (x1:[]) = error "list must have at least 2 elements"
doubleLoop' funpair (x1:x2:[]) = [funpair x1 x2]
doubleLoop' funpair (x:xs) = (singleLoop' (funpair x) xs) ++ (doubleLoop' funpair xs)

-- singleLoop funacc funone list
-- Loop over all elements in input list
--    funacc: accumulator function to apply to each successive result from all elements
--    funone: function to apply to each element
--    list: list of elements
singleLoop :: (Num b) => (b -> b -> b) -> (a -> b) -> [a] -> b
singleLoop funacc funone [] = 0
singleLoop funacc funone (x:[]) = funone x
singleLoop funacc funone (x:xs) = funacc (funone x) (singleLoop funacc funone xs)

-- this version does not accumulate the result and instead returns a list
singleLoop' :: (a -> b) -> [a] -> [b]
singleLoop'  funone [] = []
singleLoop'  funone (x:[]) = [funone x]
singleLoop'  funone (x:xs) = (funone x):(singleLoop'  funone xs)

enerPair :: [Double] -> [Double] -> [Double] -> Double -> Double
enerPair p0 p1 box rcsq 
  | r2 < rcsq = lenjones_r2 r2
  | otherwise = 0.0
    where r2 = minimg_r2 p0 p1 box

enerTotal pos box rcsq = doubleLoop (+) (\p0 p1 -> enerPair p0 p1 box rcsq) pos
--enerTotal pos box rcsq = sum $ doubleLoop' (\p0 p1 -> enerPair p0 p1 box rcsq) pos
  
enerOnePos p0 posrest box rcsq = singleLoop (+) (\p1 -> enerPair p0 p1 box rcsq) posrest
--enerOnePos p0 posrest box rcsq = sum $ singleLoop' (\p1 -> enerPair p0 p1 box rcsq) posrest
  