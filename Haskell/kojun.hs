module Kojun where

import Tabuleiros

-- Recebe o tabuleiro que deve ser resolvido e seu tamanho
-- Mapeia o tabuleiro e tenta resolver ele
-- Retorna o tabuleiro resolvido
kojun :: Tabuleiro -> Tabuleiro -> Int -> IO String
kojun valoresTabuleiro regioesTabuleiro tamanho =
    let regioesMapeadas = mapearTabuleiro regioesTabuleiro tamanho
        tabuleiroResolvido = resolverTabuleiro 0 0 tamanho valoresTabuleiro regioesTabuleiro regioesMapeadas
    in return (formatarResultado tabuleiroResolvido)


-- Recebe o tabuleiro resolvido
-- Caso vazio printa que não tem solução disponível
-- Caso solucionado printa o tabuleiro
formatarResultado :: Tabuleiro -> String
formatarResultado tabuleiroResolvido
    | null tabuleiroResolvido || all null tabuleiroResolvido = "Não há solução para esse Tabuleiro"
    | otherwise = unlines (map (unwords . map show) tabuleiroResolvido)


-- Mapeia as regioes do tabuleiro e retorna uma matriz de listas de tuplas(a, b)
-- Cada lista é uma região
-- Cada tupla é composta pelos índices linha e coluna associado associado no Tabuleiro de Valores
mapearTabuleiro :: Tabuleiro -> Int -> TabuleiroMapeado
mapearTabuleiro regioesTabuleiro tamanho =
    let regioesMapeadas = replicate (qtdeRegioes regioesTabuleiro) []
    in mapearRegioes regioesTabuleiro regioesMapeadas tamanho


-- Retorna o número de regiões diferentes (+1 Porque começa em 0)
qtdeRegioes :: Tabuleiro -> Int
qtdeRegioes regioesTabuleiro = maximum (concat regioesTabuleiro) + 1


-- Cria uma lista com todos vetores (i, j) do Tabuleiro
-- Para cada vetor chama a função atualizarRegião com o valor dele
-- Retorna um TabuleiroMapeado atualizado com uma lista daquela região mapeada
mapearRegioes :: Tabuleiro -> TabuleiroMapeado -> Int -> TabuleiroMapeado
mapearRegioes regioesTabuleiro regioesMapeadas tamanho =
    let coordenadas = [(i, j) | i <- [0..tamanho-1], j <- [0..tamanho-1]]
    in foldr (mapearRegiao regioesTabuleiro) regioesMapeadas coordenadas


-- Atualiza uma lista do TabuleiroMapeado adicionando vetores (i, j)
-- Retorna o TabuleiroMapeado atualizado 
mapearRegiao :: Tabuleiro -> Vetor -> TabuleiroMapeado -> TabuleiroMapeado
mapearRegiao regioesTabuleiro (i, j) regioesMapeadas =
    let idRegiao = regioesTabuleiro !! i !! j
        regiaoMapeada = (i, j) : (regioesMapeadas !! idRegiao)
        regiaoMapeadaAtualizada = mapearElemento idRegiao regiaoMapeada regioesMapeadas
    in regiaoMapeadaAtualizada


-- Função para atualizar cada elemento de uma lista do Tabuleiro Mapeado 
mapearElemento :: Int -> a -> Linha a -> Linha a
mapearElemento _ _ [] = []
mapearElemento 0 valorRegiao (x:xs) = valorRegiao : xs
mapearElemento n valorRegiao (x:xs) = x : mapearElemento (n - 1) valorRegiao xs


-- Loop principal que deve resolver o Tabuleiro varrendo posição por posição
-- Retorna o Tabuleiro original solucionado
resolverTabuleiro :: Int -> Int -> Int -> Tabuleiro -> Tabuleiro -> TabuleiroMapeado -> Tabuleiro
resolverTabuleiro i j tamanho valoresTabuleiro regioesTabuleiro regioesMapeadas
    -- Caso tenha percorrido toda matriz e solucionado o problema
    -- Retorna o tabuleiro solucionado
    | (i == tamanho - 1) && (j == tamanho) = valoresTabuleiro

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
        in ocuparPosicao valMax i j tamanho valoresTabuleiro regioesTabuleiro regioesMapeadas

-- Retorna o tamanho de uma região específica a partir do Tabuleiro Mapeado e o Id da Região
tamanhoRegiao :: TabuleiroMapeado -> Int -> Int
tamanhoRegiao regioesMapeadas idRegiao =
    length (regioesMapeadas !! idRegiao)


-- Usa o algoritmo de Backtracking para tentar ocupar a região com cada número possível
-- Se nenhum número for válido significa que a posição não é ocupável e o tabuleiro não é resolvível
-- Retorna um Tabuleiro Vazio ou Resolvido como resposta
ocuparPosicao :: Valor -> Int -> Int -> Int -> Tabuleiro -> Tabuleiro -> TabuleiroMapeado -> Tabuleiro
ocuparPosicao valorPosicao i j tamanho valoresTabuleiro regioesTabuleiro regioesMapeadas
    -- Se não foi possível ocupar a posição não é possível resolver o tabuleiro
    -- Retorna um tabuleiro vazio
    | valorPosicao <= 0 = [[]]

    -- Verifica se é possivel inserir aquele valor na posição
    | otherwise = 
        -- Se possível atualiza o tabuleiro e chama a função Kojun na próxima posição
        if valorPossivel valorPosicao i j tamanho valoresTabuleiro regioesTabuleiro regioesMapeadas then
            let tabuleiroAtualizado = atualizarTabuleiro valorPosicao i j valoresTabuleiro
                tabuleiro = resolverTabuleiro i (j + 1) tamanho tabuleiroAtualizado regioesTabuleiro regioesMapeadas
    
                -- Caso as próximas iterações forem bem sucedidas retorna o tabuleiro resolvido até o momento
            in  if not (null tabuleiro || all null tabuleiro) then 
                    tabuleiro
                -- Se alguma der errado tenta resolver mais uma vez essa posição com um número menor
                else 
                    ocuparPosicao (valorPosicao - 1) i j tamanho valoresTabuleiro regioesTabuleiro regioesMapeadas
        
        -- Se não chama a função mais uma vez com um valor de posição menor
        else 
            ocuparPosicao (valorPosicao - 1) i j tamanho valoresTabuleiro regioesTabuleiro regioesMapeadas

-- Modifica o valor do tabuleiro na posição (i, j) com o novo valor
atualizarTabuleiro:: Valor -> Int -> Int -> Tabuleiro -> Tabuleiro
atualizarTabuleiro valor i j valoresTabuleiro =
    let linha = valoresTabuleiro !! i
        linhaAtualizada = take j linha ++ [valor] ++ drop (j + 1) linha
    in take i valoresTabuleiro ++ [linhaAtualizada] ++ drop (i+1) valoresTabuleiro

-- Verifica se é possivel inserir o valor escolhido na posição
-- Passa por 3 verificações necessárias
-- Se todas forem válidas retorna True
valorPossivel :: Valor -> Int -> Int -> Int -> Tabuleiro -> Tabuleiro -> TabuleiroMapeado -> Bool
valorPossivel valorPosicao i j tamanho valoresTabuleiro regioesTabuleiro regioesMapeadas =
    let regiaoMapeada = regioesMapeadas !! (regioesTabuleiro !! i !! j)
    in verificarValorRegiao valorPosicao valoresTabuleiro regiaoMapeada &&
       verificarValorAdjacentes valorPosicao i j tamanho valoresTabuleiro &&
       verificarColunaRegiao valorPosicao i j tamanho valoresTabuleiro regioesTabuleiro

-- Verifica se o valor escolhido já está na região
-- Caso esteja, retorna falso
verificarValorRegiao :: Valor  -> Tabuleiro -> Linha Vetor -> Bool
verificarValorRegiao valorPosicao valoresTabuleiro regiaoMapeada =
    not (any (\(i, j) -> (valoresTabuleiro !! i !! j) == valorPosicao) regiaoMapeada)

-- Verifica se o valor da posição atual é diferente dos valores das posições adjacentes
-- Verifica os valores nas posições da direita, esquerda, inferior e superior, respectivamente
verificarValorAdjacentes :: Valor -> Int -> Int -> Int -> Tabuleiro -> Bool
verificarValorAdjacentes valorPosicao i j tamanho valoresTabuleiro =
    not ((j+1 < tamanho) && (valoresTabuleiro !! i !! (j+1) == valorPosicao)) &&
    not ((j-1 >= 0) && (valoresTabuleiro !! i !! (j-1) == valorPosicao)) &&
    not ((i+1 < tamanho) && (valoresTabuleiro !! (i+1) !! j == valorPosicao)) &&
    not ((i-1 >= 0) && (valoresTabuleiro !! (i-1) !! j == valorPosicao)) 

-- Verifica se o valor se adequa a regra da linha vertical de uma região
-- Cada valor deve ser em ordem crescente de baixo para cima
verificarColunaRegiao :: Valor -> Int -> Int -> Int -> Tabuleiro -> Tabuleiro -> Bool
verificarColunaRegiao valorPosicao i j tamanho valoresTabuleiro regioesTabuleiro =
    -- Verifica se a posição inferior está no tabuleiro e na mesma região
    -- Se sim, verifica se o valor dela é menor que o da posição
    not (((i-1) >= 0) &&
        ((regioesTabuleiro !! (i-1) !! j) == (regioesTabuleiro !! i !! j)) &&
        ((valoresTabuleiro !! (i-1) !! j) < valorPosicao))
    -- Verifica se a posição superior está no tabuleiro e na mesma região
    -- Se sim, verifica se o valor dela é maior que o da posição
    && not (((i+1) < tamanho) &&
            ((regioesTabuleiro !! (i+1) !! j) == (regioesTabuleiro !! i !! j)) &&
            ((valoresTabuleiro !! (i+1) !! j) > valorPosicao))
