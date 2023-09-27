module Main where

import Tabuleiros
import Kojun
import Data.ByteString (putStr)

main :: IO ()
main = do
    putStrLn "Digite o tamanho do tabuleiro que quer que seja resolvido 6, 8 ou 10:"
    input <- getLine
    let tamanho = read input :: Int

    putStrLn "\n"
    case tamanho of
        6  -> do
            resultadoIO <- kojun get_valores_tabuleiro6x6 get_regioes_tabuleiro6x6 tamanho
            putStrLn resultadoIO
        8  -> do 
            resultadoIO <- kojun get_valores_tabuleiro8x8 get_regioes_tabuleiro8x8 tamanho
            putStrLn resultadoIO
        10 -> do 
            resultadoIO <- kojun get_valores_tabuleiro10x10 get_regioes_tabuleiro10x10 tamanho
            putStrLn resultadoIO
        _  -> do
            putStrLn "Não existe um tabuleiro desse tamanho no banco. Tente novamente."
            main