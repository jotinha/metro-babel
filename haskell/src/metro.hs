import Debug.Trace
import Utils
import RandomUtils
import qualified Montecarlo as MC
import Structures
import System.IO

askInput :: (Read b,Show b) => String -> b -> IO b
askInput question def = do 
	putStr (question ++ ": ")
	hFlush stdout		--fix buffer not flushing until end of line
	ans <- getLine
	let (err,val) = case reads ans of
			[(x,"")] 	-> (False,x)	  --if read works, return answer
			_					-> (True,def)		--if read fails, return default value
	putStrLn $ (show val) ++ (if err then " *Read Error* Using default" else "")
	return val


main = do
	temp 			<- askInput "Temperature" (1.0 :: Double)
	n 				<- askInput "Number of particles" (108 :: Int)
	dens 			<- askInput "Density" (1.0 :: Double)
	rc 				<- askInput "Potential Cutoff Radius" (2.5 :: Double) 
	niters_eq <- askInput "Number of interations for equilibration" (100 :: Integer)
	step_eq		<- askInput	"Equilibration step" (10 :: Int)
	niters 		<- askInput "Number of iterations for run" (300 :: Integer)
	step			<- askInput "Sampling step" (10 :: Int)
	seed			<- askInput "Random Number Generator seed" (1 :: Int)

	let 	
		rl = (fromIntegral n / dens)**(1.0/3.0)
		rd = rl*0.01
		box = replicate 3 rl
		rndg0 = mkStdGen seed

		state0 = let (pos,rndg1) = iniStructureLiq n box rndg0
		         in MC.iniState pos box (rc*rc) rndg1

		chain = MC.createChain step temp rd (rc*rc) state0 
		nobs = (fromIntegral niters) `div` (fromIntegral step)		--integral division

	mapM (\state-> print (MC.energy state)) $ take nobs chain 
