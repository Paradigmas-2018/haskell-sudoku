module Cell where

import Block (searchBlock)
import Column (searchColumn)
import Row (searchRow)
import Data.List (sort, nub)

-- Busca as células associadas a uma posição dada.
searchCell :: Int -> [Int]
searchCell n = sort $ nub $ rw ++ col ++ blk
                where rw  = searchRow n
                      col = searchColumn n
                      blk = searchBlock n   
         
-- Verifica se uma célula tem valor 0, ou seja, se está vazia.                      
isZero :: Int -> [Int] -> Bool
isZero n puzzle = zero puzzle 0
        where zero _ 81 = error "out of bounds"
              zero [] _ = True
              zero (x:xs) i  = if i == n then x == 0 else zero xs (i + 1)                                

-- Identifica os valores imposssíveis para uma célula.
impossibles :: Int -> [Int] -> [Int]
impossibles n puzzle = if zero then getImp else [1..9]
    where zero = isZero n puzzle
          getImp = sort $ nub $ filter (/= 0) (getValues puzzle (searchCell n) 0)
                    where getValues [] _ _         = []
                          getValues (x:xs) cells i = if i `elem` cells then x : nextVal else nextVal
                                where nextVal = getValues xs cells (i + 1)                  
                                
-- Identifica os valores possíveis para completar a célula n.
possibles :: Int -> [Int] -> [Int]
possibles n puzzle = delete [1..9] (impossibles n puzzle)
    where delete [] _ = []
          delete (x:xs) imp = if not (elem x imp) then x : nextDel else nextDel
                where nextDel = delete xs imp

--  
solveCell :: Int -> [Int] -> Int
solveCell n puzzle = solve (possibles n puzzle)
    where solve [] = 0
          solve (v:vs) | not (v `elem` (pos searchBlock))  = v
                       | not (v `elem` (pos searchRow))  = v
                       | not (v `elem` (pos searchColumn)) = v
                       | otherwise = solve vs
          pos f = pos (filter (/= n) (f n))
                where pos [] = []
                      pos (x:xs)  = possibles x puzzle ++ pos xs                                           