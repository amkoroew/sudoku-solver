module Sudoku where

import Data.List (sort)

type Sudoku = [Int]
type Part   = [Int]
type Parts  = [Part]

main :: Sudoku
main = solve s
         where
           s = [6, 0, 8, 0, 9, 0, 2, 0, 7, 2, 9, 0, 7, 0, 4, 0, 3, 0, 0, 0, 3, 2, 6, 0, 0, 9, 1, 1, 8, 6, 0, 2, 0, 3, 0, 0, 0, 3, 0, 9, 5, 1, 0, 0, 2, 9, 0, 0, 3, 0, 0, 7, 1, 4, 0, 0, 0, 0, 7, 5, 4, 2, 8, 5, 7, 1, 0, 0, 2, 9, 0, 0, 0, 2, 4, 6, 0, 9, 0, 7, 0] 

solve :: Sudoku -> Sudoku
solve sudoku = solve' 1 sudoku sudoku
               
solve' :: Int -> Sudoku -> Sudoku -> Sudoku
solve' i s s' = if solved s
                then s
                else if elem 0 s
                     then solve' i (guess i s) s
                     else solve' (i') (guess i' s') s'
                       where
                         i' = i + 1

solved :: Sudoku -> Bool
solved s = partsSolved (rows s) && partsSolved (columns s) && partsSolved (squares s)

partsSolved :: Parts -> Bool
partsSolved p = and (map partSolved p)

partSolved :: Part -> Bool
partSolved = ([1..9] ==) . sort

guess :: Int -> Sudoku -> Sudoku
guess i (0 : xs) = i : xs
guess i (x : xs) = x : guess i xs
guess _ _        = undefined

parts :: Sudoku -> Parts
parts [] = []
parts s  = take 9 s : parts (drop 9 s)

rows :: Sudoku -> Parts
rows = parts

columns :: Sudoku -> Parts
columns s = parts [s !! (i + j) | i <- [0..8], j <- [0,9..72]]

squares :: Sudoku -> Parts
squares s = parts [s !! (i + j + k) | i <- [0,3,6], j <- [0,9..72], k <- [0..2]]
