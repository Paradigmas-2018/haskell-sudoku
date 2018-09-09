module Block where

-- Mapeia as posições de um bloco. Inicia com o bloco 0.
block :: Int -> [Int]
block n = rest startpoint 0 0 0
    where startpoint | n <= 2 = n * 3
                 | n <= 5 = 27 + (n * 3 - 9)
                 | n <= 8 = 54 + (n * 3 - 18)
          rest _ _ _ 9 = []
          rest startpoint x n i = number : rest startpoint nextX nextN (i + 1)
                where number = startpoint + n + x
                      nextX = if n == 2 then x + 9 else x
                      nextN = if n == 2 then 0 else n + 1 

-- Busca as posições do bloco a partir de uma celula.
searchBlock :: Int -> [Int]
searchBlock x = search x 0
            where search _ 9 = error "Could not find block"
                  search x n = if x `elem` blk then blk else search x (n + 1)
                                where blk = block n                      