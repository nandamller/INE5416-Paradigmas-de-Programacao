module Kojun where

import Tabuleiros

-- Recebe o tabuleiro que deve ser resolvido e seu tamanho
-- Chama a função principal que irá resolver ele
-- Retorna o tabuleiro resolvido
kojun :: Tabuleiro -> Tabuleiro -> Int -> (Bool, Tabuleiro)
kojun valoresTabuleiro regioesTabuleiro tamanho =
    resolverTabuleiro 0 0 tamanho valoresTabuleiro regioesTabuleiro  (mapearTabuleiro regioesTabuleiro (qtdeRegioes regioesTabuleiro) tamanho)


-- Retorna o número de regiões diferentes
qtdeRegioes :: Tabuleiro -> Int
qtdeRegioes regioesTabuleiro = maximum (concat regioesTabuleiro) + 1


-- Mapeia as regioes do tabuleiro e retorna uma matriz de listas de tuplas(a, b)
-- Cada lista é uma região
-- Cada tupla é composta pelos índices linha e coluna associado associado no Tabuleiro de Valores
mapearTabuleiro :: Tabuleiro -> Int -> Int -> TabuleiroMapeado
mapearTabuleiro regioesTabuleiro quantidadeRegioes tamanho =
    let regioesMapeadas = replicate quantidadeRegioes []
                        in mapearRegioes regioesTabuleiro regioesMapeadas tamanho


-- Cria uma lista com todos vetores (i, j) do Tabuleiro
-- Para cada vetor chama a função atualizarRegião com o valor dele
-- Retorna um TabuleiroMapeado atualizado com uma lista daquela região mapeada
mapearRegioes :: Tabuleiro -> TabuleiroMapeado -> Int -> TabuleiroMapeado
mapearRegioes regioesTabuleiro regioesMapeadas tamanho =
    let coordenadas = [(i, j) | i <- [0..tamanho-1], j <- [0..tamanho-1]]
                    in foldr (mapearRegiao regioesTabuleiro) regioesMapeadas coordenadas


-- Atualiza uma lista do TabuleiroMapeado adicionando um vetor (i, j)
-- Retorna o TabuleiroMapeado atualizado 
mapearRegiao :: Tabuleiro -> Vetor -> TabuleiroMapeado -> TabuleiroMapeado
mapearRegiao regioesTabuleiro (i, j) regioesMapeadas =
    let idRegiao = regioesTabuleiro !! i !! j
        regiaoMapeada = (i, j) : (regioesMapeadas !! idRegiao)
    in take idRegiao regioesMapeadas ++ [regiaoMapeada] ++ drop (idRegiao + 1) regioesMapeadas


-- Loop principal que deve resolver o Tabuleiro varrendo posição por posição
-- Retorna o Tabuleiro original solucionado
resolverTabuleiro :: Int -> Int -> Int -> Tabuleiro -> Tabuleiro -> TabuleiroMapeado -> (Bool, Tabuleiro)
resolverTabuleiro i j tamanho valoresTabuleiro regioesTabuleiro regioesMapeadas
    -- Caso tenha percorrido toda matriz e solucionado o problema
    -- Retorna o tabuleiro solucionado
    | (i == tamanho - 1) && (j == tamanho) = 
        (True, valoresTabuleiro)

    -- Varreu a linha até o último elemento
    -- Pula para a próxima linha e reinicia o processo
    | j == tamanho = 
        resolverTabuleiro (i+1) 0 tamanho valoresTabuleiro regioesTabuleiro regioesMapeadas

    -- Verifica se a posição avaliada já está ocupada
    -- Caso sim, pula para a próxima da linha
    | (valoresTabuleiro !! i !! j) > 0 = 
        resolverTabuleiro i (j+1) tamanho valoresTabuleiro regioesTabuleiro regioesMapeadas

    -- Posição não está ocupada
    -- Procura um numero para ocupá-la
    | otherwise =
        -- Procura a partir do maior número da regiao da posição (Tamanho da Região)
        let valMax = tamanhoRegiao regioesMapeadas (regioesTabuleiro !! i !! j)
        in avaliarNumeros valMax  i j valoresTabuleiro regioesTabuleiro regioesMapeadas

-- Retorna o tamanho de uma região específica a partir do Tabuleiro Mapeado e o Id da Região
tamanhoRegiao :: TabuleiroMapeado -> Int -> Int
tamanhoRegiao regioesMapeadas idRegiao =
    length (regioesMapeadas !! idRegiao)

avaliarNumeros :: Int -> Int -> Int -> Tabuleiro -> Tabuleiro -> TabuleiroMapeado -> (Bool, [[Int]])
avaliarNumeros valMax i j valoresTabuleiro regioesTabuleiro regioesMapeadas = (True, regioesTabuleiro)



