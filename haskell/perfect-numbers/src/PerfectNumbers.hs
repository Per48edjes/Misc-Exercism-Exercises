module PerfectNumbers (classify, Classification (..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

isqrt :: Int -> Int
isqrt n = floor (sqrt $ fromIntegral n :: Double)

classify :: Int -> Maybe Classification
classify n
    | n <= 0 = Nothing
    | aliquotSum == n = Just Perfect
    | aliquotSum < n = Just Deficient
    | aliquotSum > n = Just Abundant
    | otherwise = Nothing
  where
    aliquotSum = subtract n (sum $ [if d2 /= d then d2 else 0 | d <- smallDivisors, d2 <- [n `div` d]] ++ smallDivisors)
    smallDivisors = filter (\x -> n `mod` x == 0) [1 .. isqrt n]
