module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
    | length xs /= length ys = Nothing
    | otherwise = Just $ foldr incrMismatch 0 $ zip xs ys
  where
    incrMismatch (x, y) acc = if x == y then acc else acc + 1
