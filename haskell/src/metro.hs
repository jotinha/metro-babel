import System.Random
import Debug.Trace

normsq :: (Num a) => [a] -> a
normsq = sum . map (^2)

minimg_r2 p0 p1 box = normsq ( zipWith3 minimgx p0 p1 box )
 	where minimgx x0 x1 lx = dx - lx * (fromIntegral (round (dx/lx))) where dx = x1 - x0

minimg_dr p0 p1 box = sqrt (minimg_r2 p0 p1 box)
			
lenjones_dr r = 4*(_r^12 - _r^6) where _r = 1.0/r
lenjones_r2 0.0 = error "lenjones got zero distance"
lenjones_r2 r2 = 4*(_r2^6 - _r2^3) where _r2 = 1.0/r2

doubleLoopNoRepeat xs = [(a,b) | (i,a) <- zip [0..] xs, (j,b) <- zip [0..] xs, j > i]
singleLoopExceptIdx xs i = [x | (j,x) <- zip [0..] xs, j /= i]

allPairs_r2 pos box = map (\(a,b) -> minimg_r2 a b box) (doubleLoopNoRepeat pos)

mean xs = sum xs / (fromIntegral (length xs))

enerPair p0 p1 box = lenjones_r2 $ minimg_r2 p0 p1 box

enerTotal pos box = sum $ map enerPair' (doubleLoopNoRepeat pos)
          where enerPair' (p0,p1) = enerPair p0 p1 box

enerOne i pos box = sum $ map enerPair' (singleLoopExceptIdx pos i)
          where enerPair' p0 = enerPair p0 (pos !! i ) box

enerOnePos pos1 posrest box = sum $ map enerPair' posrest
          where enerPair' p0 = enerPair p0 pos1 box

seed = 12345
randomGen0 = mkStdGen seed

randomd01 :: (RandomGen g) => g -> (Double,g) 
randomd01 rndg = randomR (0.0,1.0) rndg

randomsd01 :: (RandomGen g) => Int -> g -> ([Double],g) 
randomsd01 0 rndg = ([],rndg)
randomsd01 n rndg = (d:lst,finalrndg)
          where (d,rndg1) = randomd01 rndg
                (lst,finalrndg) = randomsd01 (n-1) rndg1

accept :: (RandomGen g) => Double -> g -> (Bool,g)
accept arg rndg
  -- trace ("arg: " ++ show arg ++ " accepted: " ++ show (exp(arg) > fst (randomd01 rndg))) False = undefined
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
--op_disp :: (RealFrac a, RandomGen g) => [[a]] -> [a] -> a -> a -> g -> (Bool,a,[[a]],g)
--op_disp pos box rd temp rndg= (rndaccept,0.0,pos,newrndg)
--        where (rndaccept,newrndg) = random rndg

--mcstep :: RealFrac a => Int -> [[a]] -> [a] -> a -> a -> (Int, a, [[a]])
mcstep pos box rd temp rndg = iterate op_disp_cum (0,initialEnergy,pos,rndg)
          where 
            initialEnergy = enerTotal pos box
            natoms = length pos
            op_disp_cum (nacc,energy,curpos,currndg) = (nacc + (if accepted then 1 else 0), energy + de, newpos, newrndg)
              where
                (accepted,de,newpos,newrndg) = op_disp curpos box rd temp currndg

-- The simplest, highest level implementation of the markov chain should take a 
-- a configuration and random number generator state as input and output a new 
-- configuration and new gen state
--mcstep' (pos,gen) = let (_,_,newpos,newgen) = op_disp pos box rd temp gen in (newpos,newgen)

reshape :: Int -> [a] -> [[a]]
reshape firstdim [] = []
reshape firstdim xs
  | firstdim <= 0 = error "dimension must be positive integer"
  | otherwise = let (el,rest) = splitAt firstdim xs in el:reshape firstdim rest

iniStructureLiq n box rndg =  (pos,newrndg)
          where (flat,newrndg) = randomsd01 (3*n) rndg 
                redPos = reshape 3 flat
                pos = map (zipWith (*) box) redPos

takeEveryN :: Int -> [a] -> [a]
takeEveryN n xs = case drop (n-1) xs of 
                    (nth:rest) -> nth : takeEveryN n rest
                    [] -> []


box = replicate 3 5.0
rd = 0.1
temp = 1.0
vol box = product box
dens n box = n / (vol box)
(pos,rndg1) = iniStructureLiq 108 box randomGen0
--chain of samples taken every <step> steps times number of atoms
chain = takeEveryN (10*natoms) $ drop 1 $ mcstep pos box rd temp rndg1
        where natoms = length pos

--finiteChain = take 100 chain
      
--(nacc,energy,newpos,newrndg) = last finiteChain
main = do
  mapM (\(a,e,_,_) -> print e) chain