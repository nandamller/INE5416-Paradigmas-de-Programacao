import Data.List

-- definição de tipos
type Row a = [a]
type Matrix a = [Row a]
type Table = Matrix Int
type Values = [Int]




-- MATRIZES PARA TESTES

-- valores iniciais do puzzle 8x8 (0 significa vazio)
initial_values8x8 :: Table
initial_values8x8 = [[3,7,5,3,0,0,0,0],
                    [0,0,0,0,0,0,0,0],
                    [0,1,0,2,0,0,0,0],
                    [0,0,0,0,0,1,0,0],
                    [0,0,6,4,0,0,0,0],
                    [0,0,3,0,3,0,5,0],
                    [0,0,0,0,0,0,0,0],
                    [0,5,0,0,0,0,4,0]
                    ]

-- grupos do puzzle 8x8
groups8x8 :: Table
groups8x8 = [[1, 2, 2, 2, 2, 2, 3, 3],
             [1, 1, 2, 2, 4, 4, 5, 5],
             [6, 6, 7, 7, 7, 7,16,16],
             [6, 9, 9,14,15,15,17,16],
             [8, 9, 9, 9,15,18,17,17],
             [8,10, 9,19,18,18,18,20],
             [8,10, 9,19,18,13,21,21],
             [8, 8,11,12,12,13,13,13]
            ]

main = do
    putStrLn (show "Bem-vindo ao resolvedor de Kojun!")

    let initial_values = initial_values8x8
    let groups = groups8x8

    putStrLn (show "Valores iniciais:")
    putStrLn (show initial_values)
    putStrLn (show "Grupos do tabuleiro:")
    putStrLn (show groups)

