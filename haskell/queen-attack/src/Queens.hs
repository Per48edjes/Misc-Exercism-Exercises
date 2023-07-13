module Queens (boardString, canAttack) where

import Data.List (intersperse)
import Data.List.Split (chunksOf)

validCoord :: (Int, Int) -> Bool
validCoord (rank, file) = rank >= 0 && rank < 8 && file >= 0 && file < 8

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black =
    let rawBoard = do
            x <- [0 .. 7]
            y <- [0 .. 7]
            [if pure (x, y) == white then 'W' else if pure (x, y) == black then 'B' else '_']
     in unlines $ map (intersperse ' ') $ chunksOf 8 rawBoard

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack w@(w_rank, w_file) b@(b_rank, b_file) = validCoords && (sameRank || sameFile || sameDiagonal)
  where
    validCoords = validCoord w && validCoord b
    sameRank = w_rank == b_rank
    sameFile = w_file == b_file
    sameDiagonal = abs (w_rank - b_rank) == abs (w_file - b_file)
