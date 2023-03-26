module Luhn (isValid) where

import Data.Char (digitToInt)


isValid :: String -> Bool
isValid n 
  | length (convertToInts n) <= 1 = False
  | otherwise = (== 0) $ (`mod` 10) $ sum $ doubleFromRight $ convertToInts n
  where 
    convertToInts = map digitToInt . filter (/= ' ')
    doubleFromRight = reverse . map (\x -> if x > 9 then x - 9 else x) . zipWith (*) (cycle [1..2]) . reverse
