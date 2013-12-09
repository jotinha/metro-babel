module Forcefield(
  enerTotal,
  enerOnePos,
  enerOne,
  singleLoopExceptIdx,
  allPairs_r2,
) where

import Utils

minimg_r2 p0 p1 box = normsq ( zipWith3 minimgx p0 p1 box )
  where minimgx x0 x1 lx = dx - lx * (fromIntegral (round (dx/lx))) where dx = x1 - x0

minimg_dr p0 p1 box = sqrt (minimg_r2 p0 p1 box)
      
lenjones_dr r = 4*(_r^12 - _r^6) where _r = 1.0/r
lenjones_r2 0.0 = error "lenjones got zero distance"
lenjones_r2 r2 = 4*(_r2^6 - _r2^3) where _r2 = 1.0/r2

mapDoubleLoopNoRepeat funpair xs   = [funpair a b | (i,a) <- zip [1..] xs, b <- drop i xs]
mapSingleLoopExceptIdx funone xs i = [funone x | (j,x) <- zip [0..] xs, j /= i]

singleLoopExceptIdx xs i = [x | (j,x) <- zip [0..] xs, j /= i]

allPairs_r2 pos box = mapDoubleLoopNoRepeat (\a b -> minimg_r2 a b box) pos

enerPair p0 p1 box rcsq  = if (r2 < rcsq) then lenjones_r2 r2 else 0.0
          where r2 = minimg_r2 p0 p1 box

enerTotal pos box rcsq = sum $ mapDoubleLoopNoRepeat (\a b -> enerPair a b box rcsq) pos

enerOne i pos box rcsq = sum $ mapSingleLoopExceptIdx (\a -> enerPair a b box rcsq) pos i
          where b = pos !! i

enerOnePos pos1 posrest box rcsq = sum $ map enerPair' posrest
          where enerPair' p0 = enerPair p0 pos1 box rcsq
