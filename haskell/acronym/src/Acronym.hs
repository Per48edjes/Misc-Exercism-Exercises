module Acronym (abbreviate) where

import Data.Char (isAlpha, isLower, isUpper, toUpper)
import Data.List.Split (splitOneOf)

abbreviate :: String -> String
abbreviate xs = concat tokens
 where
  tokens = [letters (filter isAlpha w) | w <- splitOneOf " -" xs]
  letters w
    | w == "" = ""
    | all isUpper w || all isLower w = [toUpper $ head w]
    | otherwise = filter isUpper w
