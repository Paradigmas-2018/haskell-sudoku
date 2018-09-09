module Row where

-- Mapeia as posições das células das linhas de 0 a 8
row :: Int -> [Int] 
row n = 
    if n>8 then [] 
    else startpoint : rest startpoint 1
    where startpoint = (n * 9)
          rest _ 9 = []
          rest n i = n + i : rest n (i + 1)
         
-- Busca as posições da linha a partir da posição de uma célula          
searchRow :: Int -> [Int]
searchRow x = search x 0
            where search _ 9 = error "Could not find row"
                  search x n = if x `elem` rw then rw else search x (n + 1)
                                  where rw = row n
                                  
                                  