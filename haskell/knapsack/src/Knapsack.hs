module Knapsack (maximumValue) where

import qualified Data.Array as A

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue n items = go n (length items)
  where
    bounds = ((0, 0), (n, length items))
    memo = A.listArray bounds [go i j | (i, j) <- A.range bounds]

    go capacity remainingItems
        | capacity == 0 || remainingItems == 0 = 0
        | weight > capacity = memo A.! (capacity, remainingItems - 1)
        | otherwise =
            max
                (memo A.! (capacity, remainingItems - 1))
                (value + memo A.! (capacity - weight, remainingItems - 1))
      where
        -- Feel like we're constrained by the input types
        (weight, value) = items !! (remainingItems - 1)
