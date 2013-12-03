module Montecarlo (
  createChain,
  iniState,
  State(..),
  Sim(..)
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

data Sim = Sim {
  temp :: Double,
  rd :: Double
}

iniState :: [[Double]] -> [Double] -> StdGen -> State
iniState pos box rndg = State pos box (enerTotal pos box) rndg 0

natoms :: State -> Int
natoms State {pos=p} = length p

changeState :: Sim -> State -> State
changeState sim (State {pos=oldpos, energy=oldenergy, rndg=rndg0, uid=uid0, box=box}) = State pos' box energy' rndg' uid'
  where
    natoms = length oldpos
    (i,rndg1) = randomR (0, natoms -1) rndg0
    (d3, rndg2) = randomsd01 3 rndg1
    (left,posiOld:right) = splitAt i oldpos
    posiNew = zipWith (\x d -> x + (2.0*d-1.0)*(rd sim)) posiOld d3
    posrest = singleLoopExceptIdx oldpos i
    eold = enerOnePos posiOld posrest box
    enew = enerOnePos posiNew posrest box
    (accepted,rndg') = accept ((-(enew-eold))/(temp sim)) rndg2
    pos' = if accepted then (left ++ (posiNew:right)) else oldpos
    de = if accepted then (enew - eold) else 0.0
    energy' = oldenergy + de
    uid' = uid0 + (if accepted then 1 else 0)
    
accept :: (RandomGen g) => Double -> g -> (Bool,g)
accept arg rndg
  -- | trace (show arg) False = undefined
  | arg >= 0 = (True,rndg)
  | otherwise = let (trial, newrndg) = randomd01 rndg in (exp(arg) > trial, newrndg)

createChain :: Int -> Double -> Double -> State -> [State]
createChain obsStep temp rd state0
  | obsStep <= 0            = error "obsStep must be positive integer"
  | length (box state0) /= 3  = error "expected box with length 3"
  | otherwise               = takeEveryN (obsStep*natoms) $ drop 1 $ iterate changeState' state0
    where natoms = length (pos state0)
          changeState' = changeState (Sim temp rd)
