module Montecarlo (
  createChain
) where

import Utils
import RandomUtils
import Forcefield


-- The simplest, highest level implementation of the markov chain should take a 
-- a configuration and random number generator state as input and output a new 
-- configuration and new gen state
-- mcstep' (pos,gen) = let (_,_,newpos,newgen) = op_disp pos box rd temp gen in (newpos,newgen)

accept :: (RandomGen g) => Double -> g -> (Bool,g)
accept arg rndg
  | arg >= 0 = (True,rndg)
  | otherwise = let (trial, newrndg) = randomd01 rndg in (exp(arg) > trial, newrndg)


op_disp :: (RandomGen g) => [[Double]] -> [Double] -> Double -> Double -> g -> (Bool,Double,[[Double]],g)
op_disp pos box rd temp rndg = (accepted,de,newpos,newrndg)
          where natoms = length pos 
                (i,rndg1) = randomR (0, natoms -1) rndg
                (d3,rndg2) = randomsd01 3 rndg1
                (left,posiOld:right) = splitAt i pos
                posiNew = zipWith (\x d -> x + (2.0*d-1.0)*rd) posiOld d3
                posrest = singleLoopExceptIdx pos i
                eold = enerOnePos posiOld posrest box
                enew = enerOnePos posiNew posrest box
                (accepted,rndg3) = accept ((-(enew-eold))/temp) rndg2
                newpos = if accepted then (left ++ (posiNew:right)) else pos
                de = if accepted then (enew - eold) else 0.0
                newrndg = rndg3

--mcstep :: RealFrac a => Int -> [[a]] -> [a] -> a -> a -> (Int, a, [[a]])
mcstep pos box rd temp rndg = iterate op_disp_cum (0,initialEnergy,pos,rndg)
          where 
            initialEnergy = enerTotal pos box
            natoms = length pos
            op_disp_cum (nacc,energy,curpos,currndg) = (nacc + (if accepted then 1 else 0), energy + de, newpos, newrndg)
              where
                (accepted,de,newpos,newrndg) = op_disp curpos box rd temp currndg


createChain :: (RandomGen g) => Int -> [[Double]] -> [Double] -> Double -> Double -> g -> [(Int,Double,[[Double]],g)]
createChain obsStep pos box rd temp rndg
  | obsStep <= 0    = error "obsStep must be positive integer"
  | length box /= 3 = error "expected box with length 3"
  | otherwise       =   takeEveryN (obsStep*natoms) $ drop 1 $ mcstep pos box rd temp rndg
    where natoms = length pos
