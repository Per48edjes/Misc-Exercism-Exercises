module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map, adjust, fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

updateCounter :: Char -> Either String (Map Nucleotide Int) -> Either String (Map Nucleotide Int)
updateCounter 'A' (Right m) = Right $ adjust (+ 1) A m
updateCounter 'C' (Right m) = Right $ adjust (+ 1) C m
updateCounter 'G' (Right m) = Right $ adjust (+ 1) G m
updateCounter 'T' (Right m) = Right $ adjust (+ 1) T m
updateCounter _ _ = Left "Invalid nucleotide"

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = foldr updateCounter $ Right (fromList [(A, 0), (C, 0), (G, 0), (T, 0)])
