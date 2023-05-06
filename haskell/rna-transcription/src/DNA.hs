module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse fromDNA
  where
    fromDNA c
      | c == 'G' = Right 'C'
      | c == 'C' = Right 'G'
      | c == 'T' = Right 'A'
      | c == 'A' = Right 'U'
      | otherwise = Left c
