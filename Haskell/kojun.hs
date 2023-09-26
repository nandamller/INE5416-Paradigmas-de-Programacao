module Kojun where
    
import Tabuleiros

-- Recebe o tabuleiro que deve ser resolvido e seu tamanho
-- Chama a função principal que irá resolver ele
-- Retorna o tabuleiro resolvido
kojun :: Tabuleiro -> Tabuleiro -> Int -> Tabuleiro
kojun valoresTabuleiro regioesTabuleiro tamanho = 
    resolverTabuleiro 0 0  valoresTabuleiro regioesTabuleiro (definirRegioes regioesTabuleiro (qtdeRegioes regioesTabuleiro) tamanho)


-- Loop principal que deve resolver o tabuleiro entregue a partir de um método de backtracking
resolverTabuleiro :: Int -> Int -> Tabuleiro -> Tabuleiro -> TabuleiroMapeado -> (Bool, Tabuleiro)



-- Retorna o número de regiões diferentes
qtdeRegioes :: Tabuleiro -> Int
qtdeRegioes = maximum (concat matrizRegioes) + 1


-- Mapeia as regioes do tabuleiro e retorna uma matriz de listas de tuplas(a, b)
-- Cada lista é uma região
-- Cada tupla é composta pelos índices linha e coluna associado associado no Tabuleiro de Valores
mapearRegioes :: Tabuleiro -> Int -> Int -> TabuleiroMapeado
mapearRegioes regioesTabuleiro quantidadeRegioes tamanho =
    let regioesMapeadas = replicate quantidadeRegioes [] 
                        in atualizarRegioes regioesMatriz regioes tamanho


-- Cria uma lista com todos vetores (i, j) do Tabuleiro
-- Para cada vetor chama a função atualizarRegião com o valor dele
-- Retorna um TabuleiroMapeado atualizado com uma lista daquela região mapeada
atualizarRegioes :: Tabuleiro -> TabuleiroMapeado -> Int -> TabuleiroMapeado
atualizarRegioes regioesTabuleiro regioesMapeadas tamanho =
    let coordenadas = [(i, j) | i <- [0..tamanho-1], j <- [0..tamanho-1]]
                    in foldr (atualizarRegiao regioesTabuleiro) regioes coordenadas


-- Atualiza uma lista do TabuleiroMapeado adicionando um vetor (i, j)
-- Retorna o TabuleiroMapeado atualizado 
atualizarRegiao :: Tabuleiro -> Vetor -> TabuleiroMapeado -> TabuleiroMapeado
atualizarRegiao regioesTabuleiro (i, j) regioesMapeadas =
    let idRegiao = regioesTabuleiro !! i !! j
        regiaoAtualizada = (i, j) : (regioes !! idRegiao)
    in take idRegiao regioes ++ [regiaoAtualizada] ++ drop (idRegiao + 1) regioes


-- Retorna o tamanho de uma região específica a partir do Tabuleiro Mapeado
tamanhoRegiao :: TabuleiroMapeado -> Int -> Int
tamanhoRegiao regioes idRegiao =
    length (regioes !! idRegiao)




