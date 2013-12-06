module Montecarlo (
  createChain,
  iniState,
  acc,
  State(..),
  Config(..)
) where

import Utils
import RandomUtils
import Forcefield
import Debug.Trace

-- The simplest, highest level implementation of the markov chain should take a 
-- a configuration and random number generator state as input and output a new 
-- configuration and new gen state

data State = State {
  pos :: [[Double]],
  box :: [Double],
  energy :: Double,
  rndg :: StdGen,
  uid :: Int
} deriving (Show)

data Config = Config {
  temp :: Double, -- temperature
  rd :: Double,   -- max displacment
  rcsq :: Double  -- cutoff radius (squared)
}

iniState :: [[Double]] -> [Double] -> Double -> StdGen -> State
iniState pos box rcsq rndg = State pos box (enerTotal pos box rcsq) rndg 0

natoms :: State -> Int
natoms State {pos=p} = length p

changeState :: Config -> State -> State
changeState config (State {pos=oldpos, energy=oldenergy, rndg=rndg0, uid=uid0, box=box}) = State pos' box energy' rndg' uid'
  where
    natoms = length oldpos
    (i,rndg1) = randomR (0, natoms -1) rndg0
    (d3, rndg2) = randomsd01 3 rndg1
    (left,posiOld:right) = splitAt i oldpos
    posiNew = zipWith (\x d -> x + (2.0*d-1.0)*(rd config)) posiOld d3
    posrest = singleLoopExceptIdx oldpos i
    eold = enerOnePos posiOld posrest box (rcsq config)
    enew = enerOnePos posiNew posrest box (rcsq config)
    (accepted,rndg') = accept ((-(enew-eold))/(temp config)) rndg2
    pos' = if accepted then (left ++ (posiNew:right)) else oldpos
    de = if accepted then (enew - eold) else 0.0
    energy' = oldenergy + de
    uid' = uid0 + (if accepted then 1 else 0)
    
accept :: (RandomGen g) => Double -> g -> (Bool,g)
accept arg rndg
  -- | trace (show arg) False = undefined
  | arg >= 0 = (True,rndg)
  | otherwise = let (trial, newrndg) = randomd01 rndg in (exp(arg) > trial, newrndg)

createChain :: Int -> Double -> Double -> Double -> State -> [State]
createChain obsStep temp rd rcsq state0
  | obsStep <= 0            = error "obsStep must be positive integer"
  | length (box state0) /= 3  = error "expected box with length 3"
  | otherwise               = takeEveryN (obsStep*natoms) $ drop 1 $ iterate changeState' state0
    where natoms = length (pos state0)
          changeState' = changeState (Config temp rd rcsq)


-- to get the acceptance, go to the final state and check the uid, since it starts at zero
-- and is increased everytime a change occurs, then it last one equals the number of acceptances
acc :: (Fractional a) => [State] -> a
acc chain = (fromIntegral (uid (last chain))) / (fromIntegral (length chain))