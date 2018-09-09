module Sudoku where

import Grid (grid)
import Cell (solveCell)

solveSudoku :: [Int]
solveSudoku = solve grid 0
            where solve puzzle 5000 = puzzle
                  solve puzzle i = if all (/= 0) puzzle
                                   then puzzle
                                   else solve (loop puzzle puzzle 0) (i + 1)

                                   
loop :: [Int] -> [Int] -> Int -> [Int]
loop _ [] _ = []
loop puzzle (0:xs) n = solveCell n puzzle : loop puzzle xs (n + 1)
loop puzzle (x:xs) n = x : loop puzzle xs (n + 1)                  
                  
--
prettyPrint :: IO ()
prettyPrint = do
              print verticalLine
              myprint solveSudoku 0
            where myprint [] _   = do
                                   print verticalLine
                  myprint grid 3 = do
                                   print verticalLine
                                   myprint grid 0
                  myprint grid n = do
                                   print (line (take 9 grid) "|" 0)
                                   myprint (drop 9 grid) (n + 1)

                  line [] str _     = str ++ " |"
                  line x  str 3     = line x (str ++ " |") 0
                  line (x:xs) str n = line xs (str ++ " " ++ (show x)) (n + 1)

                  verticalLine = replicate 25 '-'                  