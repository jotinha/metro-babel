module Structures(
  iniStructureLiq
) where

import Utils
import RandomUtils

iniStructureLiq :: (RandomGen g) => Int -> [Double] -> g -> ([[Double]],g)
iniStructureLiq n box rndg =  (pos,newrndg)
          where (flat,newrndg) = randomsd01 (3*n) rndg 
                redPos = reshape 3 flat
                pos = map (zipWith (*) box) redPos
