-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Redundant bracket" #-}
-- {-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}
-- {-# OPTIONS_GHC -Wno-name-shadowing #-}
-- {-# OPTIONS_GHC -Wno-unused-local-binds #-}
module Lib where

import Data.Maybe (catMaybes, fromMaybe)
import Data.List (group, sort, nub)
import Control.Monad (replicateM, forM_)
import qualified Data.Set as Set

{- 
Para todos os exercícios, você pode criar funções
auxiliares se achar conveniente. Não se esqueça de dar
nome aos parâmetros que for utilizar.

Considere o seguinte tipo de dado algébrico
que representa 4 cores.
-}

data Color = Red | Green | Blue | Yellow
            deriving (Eq, Ord)

-- Exemplo de valor
c1 :: Color
c1 = Yellow

{-
Por mera questão visual, definiremos que a forma 
de apresentação delas se dará pela primeira letra da cor, 
em maiúsculo. Red será "R", Green será "G", e assim por diante.

Exercício 1: Termine a instância de Show abaixo.
Não se esqueça de apagar o undefined.
-}
instance Show Color where
    show Red    = "R"
    show Green  = "G"
    show Blue   = "B"
    show Yellow = "Y" 

{-
Considere o seguinte sinônimo de tipo
que representa uma espécie de tabuleiro
de cores. Este "tabuleiro" não necessariamente é
uma matriz quadrada.
-}

type Board = [[Color]]

-- Exemplo de tabuleiro
t1 :: Board
t1 = [[Red, Blue, Blue, Green], 
      [Yellow, Red], 
      [Blue, Green, Red]]

{-
Exercício 2: Implemente a seguinte função
que deve trocar todas as ocorrências da primeira
cor no tabuleiro pela segunda cor, mantendo todas 
as outras cores inalteradas.
-}

fill :: Color -> Color -> Board -> Board
fill _ _ [] = []
fill oldColor newColor (line:remainder) =
    (tradeInLine oldColor newColor line) : (fill oldColor newColor remainder)
    where
        tradeInLine _ _ [] = []
        tradeInLine oldColor newColor (x:xs)
            | x == oldColor = newColor : (tradeInLine oldColor newColor xs)
            | otherwise = x : (tradeInLine oldColor newColor xs)

{-
Exercício 3: Implemente a seguinte função que deve 
retornar o número de ocorrências de uma cor no tabuleiro.
-}

countColor :: Color -> Board -> Int
countColor _ [] = 0
countColor color (line:remainder) =
    (countInLine color line) + (countColor color remainder)
    where
        countInLine _ [] = 0
        countInLine color (x:xs)
            | x == color = 1 + (countInLine color xs)
            | otherwise = countInLine color xs

{-
Exercício 4: Implemente a seguinte função que deve 
converter uma letra na cor correspondente. Estaremos 
considerando a possibilidade do caractere informado
não representar uma cor.
-}

readColor :: Char -> Maybe Color
readColor 'R' = Just Red
readColor 'G' = Just Green
readColor 'B' = Just Blue
readColor 'Y' = Just Yellow
readColor _ = Nothing

{-
Exercício 5: Implemente a seguinte função que deve 
converter uma sequência de caracteres numa lista de 
possíveis cores correspondentes.
-}

readColors :: String -> [Maybe Color]
readColors = map readColor

-- readColors "BBHYGB" ~= [Just B, Just B, Nothing, Just Y, Just G, Just B] 

{-
Exercício 6: Implemente a seguinte função que deve converter
uma lista de sequências de caracteres num tabuleiro de possíveis
cores.
-}

readColorLines :: [String] -> [[Maybe Color]]
readColorLines = map readColors

{- 
    readColorLines ["BBHYGB", "JYG", "BKKGBGY"]
       ~= [[Just B,Just B,Nothing,Just Y,Just G,Just B],
           [Nothing,Just Y,Just G],
           [Just B,Nothing,Nothing,Just G,Just B,Just G,Just Y]]
-}

{-
Exercício 7: Implemente a seguinte função que deve converter
um tabuleiro de possíveis cores em um tabuleiro comum, simplesmente
eliminando todas as cores invalidadas no processo.
-}

isValidColor :: Maybe Color -> Bool
isValidColor (Just color) = color `elem` [Red, Green, Blue, Yellow]
isValidColor Nothing      = False

createBoard :: [[Maybe Color]] -> Board
createBoard = map (map fromJust . filter isValidColor)
  where fromJust (Just x) = x
        fromJust Nothing  = error "Invalid color"

{-
    createBoard (readColorLines ["BBHYGB", "JYG", "BKKGBGY"])
        ~= [[B,B,Y,G,B],
            [Y,G],
            [B,G,B,G,Y]]
-}

{-
Exercício 8: Implemente a seguinte função que lê um número n 
digitado do teclado e depois lê n linhas, retornando-as em uma lista.
-}

readLines :: IO [String]
readLines = do
  n <- readLn 
  replicateM n getLine 

{-
Exercício 9: Implemente a seguinte função que mostra na tela
a contagem de cada uma das cores, exibindo inclusive as cores
cuja contagem for zero.
-}

allColors :: Set.Set Color
allColors = Set.fromList [Red, Green, Blue, Yellow]

printCounters :: Board -> IO ()
printCounters board = do
  let colorSet = Set.fromList (concat board)
      colorCounts = map (\c -> (c, length (filter (==c) (concat board)))) (Set.toList allColors)
  mapM_ (putStrLn . (\(color, count) -> show color ++ ": " ++ show count)) colorCounts

{- 
Exercício 10: Vá ao arquivo Main.hs e faça o que se pede.
-}