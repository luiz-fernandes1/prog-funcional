module Main (main) where

import Lib (readLines, readColorLines, readColors, countColor, fill, Color (Yellow, Red), createBoard, printCounters, readColor)


{-
   NOMES:
      LUIZ FERNANDO RODRIGUES FERNANDES - 19.2.4008
      BERNARDO BRANDÃO FREGUGLIA - 16.2.4366
-}

{-
Faça os 9 exercícios no arquivo Lib.hs e depois volte à este.

EXERCÍCIO 10
Combine tudo que fez neste projeto e implemente
a seguinte função que deve: 
  1. Ler um número n do usuário.
  2. Ler n linhas.
  3. Mostrar o tabuleiro correspondente, 
     que ignorará os caracteres inválidos.
  4. Mostrar a contagem das cores no tabuleiro.
  5. Mostrar o tabuleiro correspondente trocando todos 
     os vermelhos por amarelos.
-}

main :: IO ()
main = do
   putStrLn "Digite o número de linhas e depois digite as linhas:" 
   lines <- readLines
   let board = createBoard (readColorLines lines)
   putStrLn "Tabuleiro:"
   mapM_ print board
   putStrLn "Contadores:"
   printCounters board
   let newBoard = fill Red Yellow board
   putStrLn "Novo Tabuleiro:"
   mapM_ print newBoard

{-
EXERCÍCIO OPCIONAL
Incremente esta aplicação com funcionalidades adicionais. 
Seja criativo e mantenha a boa qualidade do seu código 
e do estilo funcional. Escreva as funções no Lib.hs
e adapte a interação com o usuário no Main.hs.
-}