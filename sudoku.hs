module Sudoku where

type Row    = [Int]
type Column = [Int]
type Square = [Int]
type Sudoku = [Int]

main = solve s
         where
           s = [6, 0, 8, 0, 9, 0, 2, 0, 7, 2, 9, 0, 7, 0, 4, 0, 3, 0, 0, 0, 3, 2, 6, 0, 0, 9, 1, 1, 8, 6, 0, 2, 0, 3, 0, 0, 0, 3, 0, 9, 5, 1, 0, 0, 2, 9, 0, 0, 3, 0, 0, 7, 1, 4, 0, 0, 0, 0, 7, 5, 4, 2, 8, 5, 7, 1, 0, 0, 2, 9, 0, 0, 0, 2, 4, 6, 0, 9, 0, 7, 0] 

solve :: Sudoku -> Sudoku
solve sudoku = if solved sudoku
               then sudoku
               else solve (guess sudoku)

solved :: Sudoku -> Bool
solved s = not (elem 0 s)

guess :: Sudoku -> Sudoku
guess = undefined

rows :: Sudoku -> [Row]
rows [] = []
rows s  = take 9 s : rows (drop 9 s)

columns :: Sudoku -> [Column]
columns s = undefined

squares :: Sudoku -> [Square]
squares = undefined
