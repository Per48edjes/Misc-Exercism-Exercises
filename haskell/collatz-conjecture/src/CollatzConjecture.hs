module CollatzConjecture (collatz) where

import           Data.List


collatz :: Integer -> Maybe Integer
collatz n
  | n < 1 = Nothing
  | otherwise = Just $ genericLength $ takeWhile (> 1) $ iterate nextNumber n
    where nextNumber k = if even k then k `div` 2 else 3 * k + 1
