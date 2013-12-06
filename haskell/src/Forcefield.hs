module Forcefield(
  enerTotal,
  enerOnePos,
  enerOne,
  doubleLoopNoRepeat,
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

doubleLoopNoRepeat xs = [(a,b) | (i,a) <- zip [0..] xs, (j,b) <- zip [0..] xs, j > i]
singleLoopExceptIdx xs i = [x | (j,x) <- zip [0..] xs, j /= i]

allPairs_r2 pos box = map (\(a,b) -> minimg_r2 a b box) (doubleLoopNoRepeat pos)

enerPair p0 p1 box rcsq  = if (r2 < rcsq) then lenjones_r2 r2 else 0.0
          where r2 = minimg_r2 p0 p1 box

enerTotal pos box rcsq = sum $ map enerPair' (doubleLoopNoRepeat pos)
          where enerPair' (p0,p1) = enerPair p0 p1 box rcsq

enerOne i pos box rcsq = sum $ map enerPair' (singleLoopExceptIdx pos i)
          where enerPair' p0 = enerPair p0 (pos !! i ) box rcsq

enerOnePos pos1 posrest box rcsq = sum $ map enerPair' posrest
          where enerPair' p0 = enerPair p0 pos1 box rcsq
