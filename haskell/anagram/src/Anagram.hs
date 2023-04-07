module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)


anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (isAnagram xs)
  where isAnagram word candidate = 
          let word' = map toLower word
              candidate' = map toLower candidate in
          sort word' == sort candidate' && word' /= candidate'
