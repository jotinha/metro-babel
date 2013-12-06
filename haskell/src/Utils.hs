module Utils
( mean,
  reshape,
  takeEveryN,
  normsq,
  askInput,
) where

import System.IO


mean xs = sum xs / (fromIntegral (length xs))

reshape :: Int -> [a] -> [[a]]
reshape firstdim [] = []
reshape firstdim xs
  | firstdim <= 0 = error "dimension must be positive integer"
  | otherwise = let (el,rest) = splitAt firstdim xs in el:reshape firstdim rest


takeEveryN :: Int -> [a] -> [a]
takeEveryN n xs = case drop (n-1) xs of 
                    (nth:rest) -> nth : takeEveryN n rest
                    [] -> []

normsq :: (Num a) => [a] -> a
normsq = sum . map (^2)

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = case reads s of
  [(x,"")] -> Just x
  _        -> Nothing

askInput :: (Read b,Show b) => String -> b -> [(b -> Bool)] -> IO b
askInput question def [] = askInput question def [(\_ -> True)]   --do with always true constrain
askInput question def constrains = do
  putStr (question ++ ": ")
  hFlush stdout   --fix buffer not flushing until end of line
  val <- fmap maybeRead getLine

  let inputError =  do 
      putStrLn "* Input Error *"
      askInput question def constrains 

  
  case val of 
    Just x -> if and ( map (\c -> c x) constrains ) 
                then return x
                else inputError
    Nothing -> inputError


-- some constrains
positive a = a >0
nonpositive a = a <= 0
negative a = a <0
nonnegative a = a >= 0