module Minesweeper (annotate) where

import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S

annotate :: [String] -> [String]
annotate board = [[countToChar $ M.findWithDefault 0 (Coord (r, c)) counts | (c, _) <- zip [0 ..] row] | (r, row) <- zip [0 ..] board]
  where
    counts = M.fromSet (\_ -> -1) mineCoords `M.union` foldl' surroundMineCounter M.empty (concatMap coordNeighbors mineCoords)
    blankCoords = S.fromList $ predCoords board (== ' ')
    mineCoords = S.fromList $ predCoords board (== '*')
    surroundMineCounter m c = M.insertWith (+) c (if c `S.member` blankCoords then 1 else 0) m

newtype Coord = Coord (Int, Int) deriving (Eq, Show, Ord)

countToChar :: Int -> Char
countToChar n
    | n > 0 = head $ show n
    | n == 0 = ' '
    | otherwise = '*'

predCoords :: [String] -> (Char -> Bool) -> [Coord]
predCoords board p = [Coord (r, c) | (r, row) <- zip [0 ..] board, (c, x) <- zip [0 ..] row, p x]

coordNeighbors :: Coord -> [Coord]
coordNeighbors (Coord (r, c)) = [Coord (r + dr, c + dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], dr /= 0 || dc /= 0, r + dr >= 0, c + dc >= 0]
