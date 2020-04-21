/* Ficheiro: Projeto1.pl
 * Nome: David Emanuel Silva Belchior -- Instituto Superior Tecnico -- LEIC-A -- Numero 95550 -- Logica para Programacao
 * Descricao: Criacao de um jogo de palavras cruzadas, com diversos predicados destinados a sua composicao.
 */


/* Predicado obtem_letras_palavras/2 - Transforma uma 
lista de palavras numa lista de listas de chars. */

obtem_letras_palavras(Lst_Pals, Letras) :-
    sort(Lst_Pals, Lst_Ord), /* Ordena a lista inicial alfabeticamente, para depois separar cada palavra. */
    obtem_ja_ordenado(Lst_Ord, Letras). 

obtem_ja_ordenado([], []). /* Caso terminal da parte recursiva do predicado original. */

obtem_ja_ordenado([P1|R1], [P2|R2]) :-
    atom_chars(P1, P2), /* Transformacao de cada palavra numa lista de chars. */
    obtem_ja_ordenado(R1, R2).


/* Predicado espaco_fila/2 - Transforma uma fila em 
listas de espacos, as quais sao enumeradas uma a uma. */

espaco_fila(Fila, Esp) :- 
    lista_espacos(Fila, L_Esp),
    member(Esp, L_Esp).

lista_espacos(Lista, Res) :- 
    lista_espacos(Lista, [], [], Res).

lista_espacos([], [], Res, Res) :-
    !.

lista_espacos([], Acum, L, Res) :-
    length(Acum, N),
    N >= 3,
    append(L, [Acum], Nova_L),
    !,
    lista_espacos([], [], Nova_L, Res).

lista_espacos([P|R1], Acum, L, Res) :-
    P == #,
    length(Acum, N),
    N >= 3,
    !,
    append(L, [Acum], Nova_L),
    lista_espacos(R1, [], Nova_L, Res).

lista_espacos([P|R1], _, L, Res) :-
    P == #,
    !,
    lista_espacos(R1, [], L, Res).

lista_espacos([P|R], Acum, L, Res) :-
    append(Acum, [P], Novo_Acum),
    lista_espacos(R, Novo_Acum, L, Res).


/* Predicado espacos_fila/2 - Transforma
uma linha numa lista de espacos. */

espacos_fila(Fila, []) :-
    \+espaco_fila(Fila, _),
    !.

espacos_fila(Fila, Espacos) :- 
    bagof(X, espaco_fila(Fila, X), Espacos).


/* Predicado espacos_puzzle/2 - Permite obter a lista de espacos que podem ser 
obtidos da grelha original (por linhas) e da grelha transposta (por colunas). */

:- use_module(library(clpfd)).

mat_transposta(Matriz, Transp) :-
    transpose(Matriz, Transp).

espacos_linhas(L1, L2) :- 
    espacos_linhas(L1, [], L2).

espacos_linhas([], Aux, Aux) :-
    !.

espacos_linhas([P|R], Aux, L2) :-
    espacos_fila(P, Esp),
    Esp \== [],
    !,
    append(Aux, Esp, Novo_Aux),
    espacos_linhas(R, Novo_Aux, L2).

espacos_linhas([_|R], Aux, L2) :-
    espacos_linhas(R, Aux, L2).

espacos_puzzle(Grelha, Espacos) :-
    espacos_linhas(Grelha, Linhas),
    mat_transposta(Grelha, Grelha_Tposta),
    espacos_linhas(Grelha_Tposta, Colunas),
    append(Linhas, Colunas, Espacos).


/* Predicado espacos_com_posicoes_comuns/3 - O terceiro 
argumento corresponde a lista de elementos do primeiro 
argumento nao identicos ao segundo argumento, mas
com elementos comuns aos deste. */

espacos_com_posicoes_comuns(Espacos, Esp, Esps_Com) :- 
    espacos_com_posicoes_comuns(Espacos, Esp, [], Esps_Com).

espacos_com_posicoes_comuns([], _, Aux, Aux) :-
    !.

espacos_com_posicoes_comuns(_, [], [], []) :-
    !.

espacos_com_posicoes_comuns([P|R], Esp, Aux, Res) :-
    intersecao(P, Esp, Intsec),
    Intsec \== [],
    P \== Esp,
    !,
    append(Aux, [P], Aux_Novo),
    espacos_com_posicoes_comuns(R, Esp, Aux_Novo, Res).

espacos_com_posicoes_comuns([_|R], Esp, Aux, Res) :-
    espacos_com_posicoes_comuns(R, Esp, Aux, Res).

intersecao(L1, L2, Res) :-
    intersecao(L1, L2, [], Res).

intersecao([], _, Res, Res) :-
    !.

intersecao(_, [], Res, Res) :-
    !.

intersecao([P|R1], L2, Lst, Res) :-
    membro_vars(P, L2),
    !,
    append(Lst, [P], Nova_Lst),
    intersecao(R1, L2, Nova_Lst, Res).

intersecao([_|R1], L2, Lst, Res) :-
    intersecao(R1, L2, Lst, Res).

membro_vars(X, [P1|_]) :-
    var(X),
    X == P1,
    !.

membro_vars(X, [_|R1]) :-
    membro_vars(X, R1).


/* Predicado palavra_possivel_esp/4 */

palavra_possivel_esp(Pal, Esp, Espacos, Lst_Letras) :-
    member(Pal, Lst_Letras),
    member(Esp, Espacos),
    espacos_com_posicoes_comuns(Espacos, Esp, Lst_Com),
    Esp = Pal,
    copy_term(Lst_Com, Copia),
    correto_comuns(Copia, Lst_Letras).

correto_comuns([], _).
    
correto_comuns([P1|R1], Lst) :-
    member(P1, Lst),
    select(P1, Lst, Nova_Lst),
    !,
    correto_comuns(R1, Nova_Lst).


/* Predicado palavras_possiveis_esp/4 */

palavras_possiveis_esp(Lst_Letras, Espacos, Esp, Pals_Possiveis) :-
    findall(Pal, palavra_possivel_esp(Pal, Esp, Espacos, Lst_Letras), Pals_Possiveis).

/* Predicado palavras_possiveis/3 */

palavras_possiveis(Letras, Espacos, Pals_Possiveis) :-
    copy_term(Espacos, Copia),
    palavras_possiveis(Letras, Copia, [], Pals_Possiveis).

palavras_possiveis(_, [], Res, Res).

palavras_possiveis(Letras, [P1|R1], Aux, Res) :-
    palavras_possiveis_esp(Letras, [P1|R1], P1, Poss_Esp),
    append(Aux, [[P1,Poss_Esp]], Novo_Aux),
    palavras_possiveis(Letras, R1, Novo_Aux, Res).