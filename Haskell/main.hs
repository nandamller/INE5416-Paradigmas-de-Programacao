module Main where

import Tabuleiros
import Kojun

main = do
    putStrLn "Digite o tamanho do tabuleiro que quer que seja resolvido 6, 8 ou 10:"
    input <- getLine
    let tamanho = read input :: Int

    case tamanho of
        6  -> mapM_ print (kojun get_valores_tabuleiro6x6 get_regioes_tabuleiro6x6 tamanho)
        8  -> mapM_ print (kojun get_valores_tabuleiro8x8 get_regioes_tabuleiro8x8 tamanho)
        10 -> mapM_ print (kojun get_valores_tabuleiro10x10 get_regioes_tabuleiro10x10 tamanho)
        _       -> do
            putStrLn "Não existe um tabuleiro desse tamanho no banco. Tente novamente."
            main