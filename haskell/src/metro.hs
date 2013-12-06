import Utils
import RandomUtils
import qualified Montecarlo as MC
import Structures
import System.IO
import Text.Printf

askInput :: (Read b,Show b) => String -> b -> IO b
askInput question def = do 
  putStr (question ++ ": ")
  hFlush stdout   --fix buffer not flushing until end of line
  ans <- getLine
  let (err,val) = case reads ans of
      [(x,"")]  -> (False,x)    --if read works, return answer
      _         -> (True,def)   --if read fails, return default value
  putStrLn $ (show val) ++ (if err then " *Read Error* Using default" else "")
  return val


main = do
  temp      <- askInput "Temperature" (1.0 :: Double)
  n         <- askInput "Number of particles" (108 :: Int)
  dens      <- askInput "Density" (1.0 :: Double)
  rc        <- askInput "Potential Cutoff Radius" (2.5 :: Double) 
  niters_eq <- askInput "Number of interations for equilibration" (100 :: Integer)
  step_eq   <- askInput "Equilibration step" (10 :: Int)
  niters    <- askInput "Number of iterations for run" (300 :: Integer)
  step      <- askInput "Sampling step" (10 :: Int)
  seed      <- askInput "Random Number Generator seed" (1 :: Int)

  let   
    rcsq = rc*rc
    rl = (fromIntegral n / dens)**(1.0/3.0)
    rd0 = rl*0.01
    box = replicate 3 rl
    rndg0 = mkStdGen seed

    nobs = (fromIntegral niters) `div` (fromIntegral step)    --integral division
    nobs_eq = (fromIntegral niters_eq) `div` (fromIntegral step_eq)   --integral division

    state0 = let (pos,rndg1) = iniStructureLiq n box rndg0
             in MC.iniState pos box rcsq rndg1
  
    ---- equilibration (update rd every step_eq steps)
    equilibrationChain = iterate updaterd (state0,rd0)
      where 
        updaterd (_state,_rd) = (_state',_rd')
          where
            _chain = take 1 $ MC.createChain step_eq temp _rd rcsq _state
            _state' = head _chain 
            _rd' = if (MC.acc _chain) > 0.5 then (_rd*1.05) else (_rd*0.95)

    -- go to last state in equilibration chain
    (state1,rd1) = equilibrationChain !! nobs_eq

    ---- run chain
    runChain = MC.createChain step temp rd1 rcsq state1

  putStrLn "Equilibration"
  mapM (\(i,(state,rd)) -> do
    printf "%i\t%g\t%.3f\n" i (MC.energy state) rd
    ) $ zip [0,(fromIntegral step_eq)..niters_eq] equilibrationChain 
  
  putStrLn "Run"
  mapM (\(i,state)-> do
    printf "%i\t%g\n" i (MC.energy state)
    ) $ zip [0,(fromIntegral step)..niters] runChain
  
  return ()
