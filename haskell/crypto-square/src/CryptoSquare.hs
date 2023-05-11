{-# OPTIONS_GHC -Wno-type-defaults #-}

module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)
import Data.List.Split (chunksOf)

encode :: String -> String
encode xs = unwords cipher
  where
    normalized = map toLower $ filter isAlphaNum xs
    c = ceiling $ sqrt $ fromIntegral $ length normalized
    r = ceiling $ fromIntegral (length normalized) / fromIntegral c
    cipher = map (\w -> w ++ replicate (r - length w) ' ') $ transpose $ chunksOf c normalized
