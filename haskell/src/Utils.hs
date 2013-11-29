module Utils
( mean,
  reshape,
  takeEveryN,
  normsq,
) where

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
