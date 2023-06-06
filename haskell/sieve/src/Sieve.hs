module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.

import Data.Set (Set, fromAscList, notMember, unions)
import Prelude hiding (div, divMod, mod, quot, quotRem, rem, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n = filter (`notMember` sieve) [2 .. n]
  where
    sieve = unions $ map (`getMultiples` n) [2 .. n]

getMultiples :: Integer -> Integer -> Set Integer
getMultiples m limit = fromAscList $ takeWhile (<= limit) $ map (* m) [2 ..]
