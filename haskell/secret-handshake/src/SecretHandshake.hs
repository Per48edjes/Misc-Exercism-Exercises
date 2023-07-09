module SecretHandshake (handshake) where

import Data.Bits (testBit)

handshake :: Int -> [String]
handshake n
    | n < 0 || n > 31 = []
    | testBit n 4 = reverse actions
    | otherwise = actions
  where
    actions = foldr f [] $ zip [0 ..] ["wink", "double blink", "close your eyes", "jump"]
    f (i, s) acc = if testBit n i then s : acc else acc
