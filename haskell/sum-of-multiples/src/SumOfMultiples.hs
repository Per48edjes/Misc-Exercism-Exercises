module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . nub $ concatMap (filter (< limit)) ([[f * i | i <- [0 .. limit]] | f <- factors])
