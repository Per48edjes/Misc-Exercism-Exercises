module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
    | n <= 0 = Nothing
    | otherwise = Just $ primes !! (n - 1)
  where
    primes = filter isPrime [1 ..]

isPrime :: Integer -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = null [x | x <- [2 .. isqrt n], n `mod` x == 0]
  where
    isqrt :: Integer -> Integer
    isqrt m = floor (sqrt $ fromIntegral m :: Double)
