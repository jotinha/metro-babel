import Debug.Trace
import Utils
import RandomUtils
import Montecarlo
import Structures

n = 108 :: Int
temp = 1.0 :: Double
rd = 0.1 :: Double
dens = 1.0 :: Double
seed = 12345

rl = (fromIntegral n / dens)**(1.0/3.0)

box = replicate 3 rl
rndg0 = mkStdGen seed
(pos,rndg1) = iniStructureLiq n box rndg0
chain = createChain 10 pos box rd temp rndg1

main = do
  mapM (\(a,e,_,_) -> print e) chain
