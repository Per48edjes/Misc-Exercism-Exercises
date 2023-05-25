{-# LANGUAGE FlexibleInstances #-}

module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map, fromList, fromListWith, toList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

newtype NucleotideCounter = NucleotideCounter (Either String (Map Nucleotide Int))
    deriving (Eq, Show)

updateCounter :: Char -> NucleotideCounter
updateCounter 'A' = NucleotideCounter $ Right $ fromList [(A, 1)]
updateCounter 'C' = NucleotideCounter $ Right $ fromList [(C, 1)]
updateCounter 'G' = NucleotideCounter $ Right $ fromList [(G, 1)]
updateCounter 'T' = NucleotideCounter $ Right $ fromList [(T, 1)]
updateCounter _ = NucleotideCounter $ Left "Invalid nucleotide"

instance Semigroup NucleotideCounter where
    (NucleotideCounter (Right m1)) <> (NucleotideCounter (Right m2)) = NucleotideCounter (Right $ fromListWith (+) (toList m1 ++ toList m2))
    (NucleotideCounter (Left s)) <> _ = NucleotideCounter (Left s)
    _ <> (NucleotideCounter (Left s)) = NucleotideCounter (Left s)

instance Monoid NucleotideCounter where
    mempty = NucleotideCounter $ Right $ fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = getCounter . foldMap updateCounter
  where
    getCounter (NucleotideCounter c) = c
