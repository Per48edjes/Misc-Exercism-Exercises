module DNA (toRNA) where

import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set   as Set


nucleotideMap :: Map.Map Char Char
nucleotideMap = Map.fromList [('G', 'C'), ('C', 'G'), ('T', 'A'), ('A', 'U')]

correspondingNucleotide :: Char -> Char
correspondingNucleotide c = fromMaybe c (Map.lookup c nucleotideMap)

toRNA :: String -> Either Char String
toRNA xs
  | isInvalidSequence = Left $ head . dropWhile (`Map.member` nucleotideMap) $ xs
  | otherwise         = Right transcribedRNA
  where generateDNA x acc   = correspondingNucleotide x : acc
        transcribedRNA      = foldr generateDNA [] xs
        isInvalidSequence   = not . null $ Set.fromList xs Set.\\ Map.keysSet nucleotideMap
