module Column where

-- Mapeia as posições das células das colunas de 0 a 8
column :: Int -> [Int]
column n = 
      if n>8 then [] else rest n 0
         where rest _ 9 = [] 
               rest n i = n : rest (n + 9) (i + 1)

-- Busca as posições da coluna a partir da posição de uma célula               
searchColumn :: Int -> [Int]
searchColumn x = search x 0
            where search _ 9 = error "Could not find column"
                  search x n = if x `elem` col then col else search x (n + 1)
                              where col = column n
                                               
                                               