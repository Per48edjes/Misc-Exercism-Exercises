{-# LANGUAGE ImportQualifiedPost #-}

module WordCount (wordCount) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T

type WordCounter = M.Map T.Text Int
type WordFrequency = [(String, Int)]

cleanString :: String -> [T.Text]
cleanString = concatMap (filter ((/=) T.empty) . T.split splitPunc . T.dropAround surroundingPunc) . T.words . T.toLower . T.pack
  where
    splitPunc c = c `elem` [',', '.', '!', '&', '@', '$', '%', '^', ':', ';', '?', '.']
    surroundingPunc c = c `elem` ['\'', '\"']

wordCount :: String -> WordFrequency
wordCount xs = do
    (k, v) <- M.toList $ foldr countWord M.empty $ cleanString xs
    pure (T.unpack k, v)
  where
    countWord :: T.Text -> WordCounter -> WordCounter
    countWord k = M.insertWith (+) k 1
