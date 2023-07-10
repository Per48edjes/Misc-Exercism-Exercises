module Triangle (rows) where

-- binom :: (Integral a) => a -> a -> a
-- binom = loop 1 1
--   where
--     loop rn rd _ 0 = rn `div` rd
--     loop _ _ 0 _ = 0
--     loop rn rd n k = loop (rn * n) (rd * k) (n - 1) (k - 1)
--
-- rows :: Int -> [[Integer]]
-- rows x = foldr step [] [0 .. toInteger x - 1]
--   where
--     step n acc = [binom n k | k <- [0 .. n]] : acc

rows :: Int -> [[Integer]]
rows x
    | x == 0 = []
    | x > 0 = take x $ iterate ((:) 1 . add) [1]
    | otherwise = error "Row number must be non-negative"

add :: [Integer] -> [Integer]
add [] = error "Add should not be called with empty list"
add [n] = [n]
add (n1 : n2 : nr) = (n1 + n2) : add (n2 : nr)
