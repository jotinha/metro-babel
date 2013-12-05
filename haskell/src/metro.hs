import Debug.Trace
import Utils
import RandomUtils
import qualified Montecarlo as MC
import Structures

data Query a = Query {
	question :: String,
	answer :: a
} deriving (Show, Eq)

questions= [Query "Temperature" 1.0,
			Query "Number of particles" 108,
			Query "Density" 1.0,
			Query "Number of interations for equilibration" 100,
			Query "Equilibration step" 10,
			Query "Number of iterations for run" 300,
			Query "Sampling step" 10]

--readAnswer query = do 
--	qq <- question query
--	putStr qq ++ ": "
--	qa <- getLine
--	q2 <- Query qq (read qa :: (answer query))
--	return q2

--readInput = mapM (\q -> do 
--	putStr (question q) ++ ": "
--	ans <- getLine
--	return (Query (question q) (read ans :: Int))) questions


n = 108 :: Int
temp = 1.0 :: Double
rd = 0.1 :: Double
dens = 1.0 :: Double
seed = 12345
rc = 2.5

rl = (fromIntegral n / dens)**(1.0/3.0)

box = replicate 3 rl
rndg0 = mkStdGen seed

state0 = let (pos,rndg1) = iniStructureLiq n box rndg0
         in MC.iniState pos box (rc*rc) rndg1

chain = MC.createChain 10 temp rd (rc*rc) state0 

main = do
  mapM (\state-> print (MC.energy state)) $ take 5 chain 
