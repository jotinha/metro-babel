module Montecarlo (
  createChain
) where

import Utils
import RandomUtils
import Forcefield
import Debug.Trace


-- The simplest, highest level implementation of the markov chain should take a 
-- a configuration and random number generator state as input and output a new 
-- configuration and new gen state

accept :: (RandomGen g) => Double -> g -> (Bool,g)
accept arg rndg
  -- | trace (show arg) False = undefined
  | arg >= 0 = (True,rndg)
  | otherwise = let (trial, newrndg) = randomd01 rndg in (exp(arg) > trial, newrndg)

createChain :: (RandomGen g) => Int -> [[Double]] -> [Double] -> Double -> Double -> g -> [(Int,Double,[[Double]],g)]
createChain obsStep pos box rd temp rndg
  | obsStep <= 0    = error "obsStep must be positive integer"
  | length box /= 3 = error "expected box with length 3"
  | otherwise       =   takeEveryN (obsStep*natoms) $ drop 1 $ iterate stateChange (0,initialEnergy,pos,rndg)
          where 
            natoms = length pos
            initialEnergy = enerTotal pos box 
            stateChange (nacc,oldenergy,oldpos,oldrndg) = (nacc + (if accepted then 1 else 0), oldenergy + de, newpos, newrndg)
              where 
                (i,rndg1) = randomR (0, natoms -1) oldrndg
                (d3,rndg2) = randomsd01 3 rndg1
                (left,posiOld:right) = splitAt i oldpos
                posiNew = zipWith (\x d -> x + (2.0*d-1.0)*rd) posiOld d3
                posrest = singleLoopExceptIdx oldpos i
                eold = enerOnePos posiOld posrest box
                enew = enerOnePos posiNew posrest box
                (accepted,rndg3) = accept ((-(enew-eold))/temp) rndg2
                newpos = if accepted then (left ++ (posiNew:right)) else oldpos
                de = if accepted then (enew - eold) else 0.0
                newrndg = rndg3



