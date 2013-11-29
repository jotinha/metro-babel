module RandomUtils(
  module System.Random,
  randomd01,
  randomsd01,
) where

import System.Random

-- create a random double between 0 and 1
randomd01 :: (RandomGen g) => g -> (Double,g) 
randomd01 rndg = randomR (0.0,1.0) rndg


-- create a list of n random doubles between 0 and 1
randomsd01 :: (RandomGen g) => Int -> g -> ([Double],g) 
randomsd01 0 rndg = ([],rndg)
randomsd01 n rndg = (d:lst,finalrndg)
          where (d,rndg1) = randomd01 rndg
                (lst,finalrndg) = randomsd01 (n-1) rndg1
