#!/usr/bin/env runhaskell

import Data.Array
import Data.List
import System.Random.Shuffle
import System.Random

-- As marcas no tabuleiro são representadas por Ints no intervalo 0..9, onde 0 representa "vazio".
type Mark = Int

-- Cada quadrado é representado por um par (linha, coluna)
type Location = (Int, Int)

-- O sudoku é uma matriz 9x9 
type Board = Array Location Mark

puzzleBoard :: Board
puzzleBoard = array ((0, 0), (8, 8)) $ puzzleAssocs zeroPuzzle

zeroPuzzle :: [[Mark]]
zeroPuzzle = [[0, 0, 0,  0, 0, 0,  0, 0, 0],
                 [0, 0, 0,  0, 0, 0,  0, 0, 0],
                 [0, 0, 0,  0, 0, 0,  0, 0, 0],

                 [0, 0, 0,  0, 0, 0,  0, 0, 0],
                 [0, 0, 0,  0, 0, 0,  0, 0, 0],
                 [0, 0, 0,  0, 0, 0,  0, 0, 0],

                 [0, 0, 0,  0, 0, 0,  0, 0, 0],
                 [0, 0, 0,  0, 0, 0,  0, 0, 0],
                 [0, 0, 0,  0, 0, 0,  0, 0, 0]]

-- Retorna todas as soluções
solutions :: Board -> [Board]
solutions b = solutions' (emptyLocations b) b
  where
    -- Dada a lista de locais vazios em uma placa, escolha um local vazio,
    -- determine quais marcas podem ser colocadas nesse local e, em seguida,
    -- recursivamente encontrar todas as soluções para esse conjunto de marcas.
    solutions' :: [Location] -> Board -> [Board]
    solutions' []     b = [b]
    solutions' (x:xs) b = concatMap (solutions' xs) candidateBoards
      where
        candidateMarks  = [m | m <- [1..9], isPossibleMark m x b]
        candidateBoards = map (\m -> copyWithMark m x b) candidateMarks

-- Retorna a lista de locais onde o valor é 0
emptyLocations :: Board -> [Location]
emptyLocations b = [(row, col) | row <- [0..8], col <- [0..8], b ! (row, col) == 0]

-- Determina se a marca pode ser colocada na posição específica
isPossibleMark :: Mark -> Location -> Board -> Bool
isPossibleMark m (row, col) b = notInRow && notInColumn && notInBox
  where
    notInRow    = notElem m $ b `marksInRow` row
    notInColumn = notElem m $ b `marksInColumn` col
    notInBox    = notElem m $ b `marksIn3x3Box` (row, col)

-- Retorna o valor do local na 
copyWithMark :: Mark -> Location -> Board -> Board
copyWithMark mark (row, col) b = b // [((row, col), mark)]

-- Retorna as marcas da linha
marksInRow :: Board -> Int -> [Mark]
b `marksInRow` row = [b ! loc | loc <- range((row, 0), (row, 8))]

-- Retorna as marcas da coluna
marksInColumn ::  Board -> Int -> [Mark]
b `marksInColumn` col = [b ! loc | loc <- range((0, col), (8, col))]

-- Retorna as marcas da box 3x3 box 
marksIn3x3Box :: Board -> Location -> [Mark]
b `marksIn3x3Box` (row, col) = [b ! loc | loc <- locations]
  where
    row' = (row `div` 3) * 3
    col' = (col `div` 3) * 3
    locations = range((row', col'), (row' + 2, col' + 2))

-- Converte uma lista de marcas em uma lista de associação de vetores
puzzleAssocs :: [[Mark]] -> [(Location, Mark)]
puzzleAssocs = concatMap rowAssocs . zip [0..8]
  where
    rowAssocs :: (Int, [Mark]) -> [((Int, Int), Mark)]
    rowAssocs (row, marks) = colAssocs row $ zip [0..8] marks

    colAssocs :: Int -> [(Int, Mark)] -> [((Int, Int), Mark)]
    colAssocs row cols = map (\(col, m) -> ((row, col), m)) cols

-- Gera uma lista embaralhada de números de 1 a 9
generateRandomMarks :: Int -> [Mark]
generateRandomMarks n = shuffle' [1..9] 9 (mkStdGen n)

-- Gera os pares da localização dos boxes da diagonal
locationsToFill :: [Location]
locationsToFill = [(x, y) | x <- [0..2], y <- [0..2]] ++ [(x, y) | x <- [3..5], y <- [3..5]] ++ [(x, y) | x <- [6..8], y <- [6..8]]

-- Gera o Board com os boxes da diagonal
generateBoxes :: Board -> Board
generateBoxes puzzle = puzzle // zip locationsToFill ( generateRandomMarks 1 ++ generateRandomMarks 2 ++ generateRandomMarks 3 )

-- Remove alguns valores para o preenchimento
removeMarks :: Board -> Board
removeMarks puzzle = puzzle // zip ( zip ( generateRandomLocations 4 )  ( generateRandomLocations 5 ) ) ( replicate 9 0 )

-- Gera as localizações aleatórias dos valores que vão ser removidos
generateRandomLocations :: Int -> [Int]
generateRandomLocations n = shuffle' [0..8] 9 (mkStdGen n)

-- Gera um Board para ser resolvido
generate :: Board
generate = removeMarks $ head $ solutions $ generateBoxes puzzleBoard