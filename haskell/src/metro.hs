import Debug.Trace
import Utils
import RandomUtils
import Montecarlo
import Structures


seed = 12345
rndg0 = mkStdGen seed
box = replicate 3 5.0
rd = 0.1
temp = 1.0
vol box = product box
dens n box = n / (vol box)
(pos,rndg1) = iniStructureLiq 108 box rndg0
chain = createChain 10 pos box rd temp rndg1
--chain of samples taken every <step> steps times number of atoms


        --where natoms = length pos

--finiteChain = take 100 chain
      
--(nacc,energy,newpos,newrndg) = last finiteChain
main = do
  mapM (\(a,e,_,_) -> print e) chain