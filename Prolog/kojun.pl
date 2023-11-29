:- use_module(library(clpfd)).

/*tabuleiro nr.1 6x6: <https://www.janko.at/Raetsel/Kojun/001.a.htm>
representado por uma lista de listas*/
tabuleiro([[[1,2],[1,_],[2,_],[2,_],[2,1],[3,_]],
            [[4,_],[4,_],[4,_],[4 ,3],[4,_],[3,_]],
            [[5,_],[6,3],[6,_],[6,_],[4,5],[7,3]],
            [[5,_],[5,_],[5,_],[6,_],[7,_],[7,_]],
            [[8,_],[8,_],[10,3],[0,_],[0,4],[0,2]],
            [[9,_],[9,_],[10,_],[10,_],[0,_],[0,_]]]).

/*mapeamento das regiões com sua respectiva quantidade de elementos (numero região, tamanho)*/
regiao_quantidade(0,5).
regiao_quantidade(1,2).
regiao_quantidade(2,3).
regiao_quantidade(3,2).
regiao_quantidade(4,6).
regiao_quantidade(5,4).
regiao_quantidade(6,4).
regiao_quantidade(7,3).
regiao_quantidade(8,2).
regiao_quantidade(9,2).
regiao_quantidade(10,3).

/*função principal que aplica todas as regras*/
kojun(Desafio) :-
    append(Desafio, Lista),
    
    %chama a regra de valor máximo para cada elemento da Lista (região)
    maplist(valor_maximo_regiao, Lista),
    
    %chama a regra de vizinhos serem diferentes para as linhas do tabuleiro
    maplist(vizinhos_diferentes, Desafio),

    %inverte linhas por colunas para chamar a regra de vizinhos novamente
    transpose(Desafio, Columns),
    maplist(vizinhos_diferentes, Columns),

    %chama a regra de superior maior para as colunas de mesmo grupo
    maplist(superior_maior, Columns),

    %agrupando os elementos de uma mesma região
    agrupa(0, Lista, Grupo0),
    agrupa(1, Lista, Grupo1),
    agrupa(2, Lista, Grupo2),
    agrupa(3, Lista, Grupo3),
    agrupa(4, Lista, Grupo4),
    agrupa(5, Lista, Grupo5),
    agrupa(6, Lista, Grupo6),
    agrupa(7, Lista, Grupo7),
    agrupa(8, Lista, Grupo8),
    agrupa(9, Lista, Grupo9),
    agrupa(10, Lista, Grupo10),

    %agrupa todas as listas de regiões em uma lista
    Grupos = [Grupo0,Grupo1,Grupo2,Grupo3,Grupo4,Grupo5,Grupo6,Grupo7,Grupo8,Grupo9,Grupo10],

    %chama a regra que verifica se todos os elementos dentro de uma mesma região são diferentes
    todos_diferentes_regiao(Grupos), !.


/*define o maior valor que os valores de cada regiao podem assumir*/
valor_maximo_regiao([R,X]) :- regiao_quantidade(R,T), X in 1..T.


/*verifica se o vizinho a direita eh diferente*/
vizinhos_diferentes([[_,_]]).
vizinhos_diferentes([[_,X1],[R2,X2]|T]) :-
    X1 #\= X2, append([[R2,X2]],T,L), vizinhos_diferentes(L).


/*verifica se o valor acima de outro eh maior, se fizerem parte do mesmo grupo*/
superior_maior([[_,_]]).
superior_maior([[R1,X1],[R2,X2]|T]) :-
    ((R1 #\= R2);
    (X1 #> X2)), append([[R2,X2]],T,L), superior_maior(L).


/*agrupa os elementos de mesmo grupo (recebe o grupo)*/
agrupa(_, [], []).
agrupa(R, [[R1, X1] | T], [X1 | L]) :- R #= R1, agrupa(R, T, L).
agrupa(R, [[R1, _] | T], L) :- R #\= R1, agrupa(R, T, L).


/*verifica se os membros de uma lista sao diferentes*/
todos_diferentes_regiao([H]) :-
    all_distinct(H).
todos_diferentes_regiao([H|T]) :-
    all_distinct(H),
    todos_diferentes_regiao(T).


solucao(TabuleiroResposta) :-
    tabuleiro(TabuleiroProblema), 
    kojun(TabuleiroProblema),
    extract_second_values(TabuleiroProblema, TabuleiroResposta),
    maplist(write_line, TabuleiroResposta).

write_line(Line) :-
    write(Line), nl.


% pegar o segundo elemento de cada matriz
extract_second([], []).
extract_second([[_, Second] | Rest], [Second | SecondValues]) :-
    extract_second(Rest, SecondValues).


% extrair os valores de cada linha da matriz
extract_second_values([], []).
extract_second_values([Sublist | Rest], [SecondValues | Result]) :-
    extract_second(Sublist, SecondValues),
    extract_second_values(Rest, Result).
