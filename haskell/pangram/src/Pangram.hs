module Pangram (isPangram) where

import Data.List
import Data.Char (toUpper)


isPangram :: String -> Bool
isPangram text = null $ map toUpper (letters \\ text) \\ text
  where letters = ['a' .. 'z']
