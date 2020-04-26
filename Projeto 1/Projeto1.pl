/* Ficheiro: Projeto1.pl
 * Nome: David Emanuel Silva Belchior -- Instituto Superior Tecnico -- LEIC-A -- Numero 95550 -- Logica para Programacao
 * Descricao: Criacao de um jogo de palavras cruzadas, com diversos predicados destinados a sua composicao.
 */

:- [codigo_comum].

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
    N >= 3 ->
    append(L, [Acum], Nova_L),
    lista_espacos([], [], Nova_L, Res);
    lista_espacos([], [], L, Res).

lista_espacos([P|R], Acum, L, Res) :-
    P == # ->
    (length(Acum, N),
    N >= 3 ->
    append(L, [Acum], Nova_L),
    lista_espacos(R, [], Nova_L, Res);
    lista_espacos(R, [], L, Res));
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

espacos_puzzle(Grelha, Espacos) :-
    espacos_linhas(Grelha, Linhas),
    mat_transposta(Grelha, Grelha_Tposta),
    espacos_linhas(Grelha_Tposta, Colunas),
    append(Linhas, Colunas, Espacos).

espacos_linhas(L1, L2) :- 
    espacos_linhas(L1, [], L2).

espacos_linhas([], Aux, Aux) :-
    !.

espacos_linhas([P|R], Aux, L2) :-
    espacos_fila(P, Esp),
    Esp \== [] ->
    append(Aux, Esp, Novo_Aux),
    espacos_linhas(R, Novo_Aux, L2);
    espacos_linhas(R, Aux, L2).

/* Predicado espacos_com_posicoes_comuns/3 - O terceiro 
argumento corresponde a lista de elementos do primeiro 
argumento nao identicos ao segundo argumento, mas
com elementos comuns aos deste. */

espacos_com_posicoes_comuns(Espacos, Esp, Esps_Com) :- 
    espacos_com_posicoes_comuns(Espacos, Esp, [], Esps_Com).

espacos_com_posicoes_comuns(_, [], [], []) :-
    !.

espacos_com_posicoes_comuns([], _, Aux, Aux) :-
    !.

espacos_com_posicoes_comuns([P|R], Esp, Aux, Res) :-
    P \== Esp,
    intersecao(P, Esp, Intsec),
    Intsec \== [] ->
    append(Aux, [P], Aux_Novo),
    espacos_com_posicoes_comuns(R, Esp, Aux_Novo, Res);
    espacos_com_posicoes_comuns(R, Esp, Aux, Res).

intersecao(L1, L2, Res) :-
    intersecao(L1, L2, [], Res).

intersecao([], _, Res, Res) :-
    !.

intersecao(_, [], Res, Res) :-
    !.

intersecao([P1|R1], L2, Lst, Res) :-
    membro_vars(P1, L2) ->
    append(Lst, [P1], Nova_Lst),
    intersecao(R1, L2, Nova_Lst, Res);
    intersecao(R1, L2, Lst, Res).

membro_vars(X, [P|R]) :-
    var(X),
    X == P;
    membro_vars(X, R).


/* Predicado palavra_possivel_esp/4 */

palavra_possivel_esp(Pal, Esp, Espacos, Lst_Letras) :-
    espacos_com_posicoes_comuns(Espacos, Esp, Lst_Com),
    Esp = Pal,
    duplicate_term(Lst_Com, Copia),
    correto_comuns(Copia, Lst_Letras).

correto_comuns([], _) :-
    !.
    
correto_comuns([P|R], Lst) :-
    member(P, Lst),
    select(P, Lst, Nova_Lst),
    correto_comuns(R, Nova_Lst),
    !.


/* Predicado palavras_possiveis_esp/4 */

palavras_possiveis_esp(Lst_Letras, Espacos, Esp, Pals_Possiveis) :-
    findall(Pal, (member(Pal, Lst_Letras), palavra_possivel_esp(Pal, Esp, Espacos, Lst_Letras)), Pals_Possiveis).


/* Predicado palavras_possiveis/3 */

palavras_possiveis(Letras, Espacos, Pals_Possiveis) :-
    palavras_possiveis(Letras, Espacos, Espacos, [], Pals_Possiveis),
    !.

palavras_possiveis(_, _, [], Res, Res).

palavras_possiveis(Letras, Espacos, [P|R], Aux, Res) :-
    palavras_possiveis_esp(Letras, Espacos, P, Poss_Esp),
    append(Aux, [[P,Poss_Esp]], Novo_Aux),
    palavras_possiveis(Letras, Espacos, R, Novo_Aux, Res).

/* Predicado letras_comuns/2 */

letras_comuns(Lst_Pals, Letras_Comuns) :-
    nth1(1, Lst_Pals, El),
    length(El, N),
    letras_comuns(Lst_Pals, [], 1, N, Letras_Comuns).


letras_comuns(_, Res, Ind, N, Res) :-
    Ind > N,
    !.

letras_comuns(Lst_Pals, Acum, Ind, N, Letras_Comuns) :-
    mesma_letra(Ind, Lst_Pals, X) ->
    append(Acum, [(Ind, X)], Novo_Acum),
    Ind_mais_1 is Ind+1,
    letras_comuns(Lst_Pals, Novo_Acum, Ind_mais_1, N, Letras_Comuns);
    Ind_mais_1 is Ind+1,
    letras_comuns(Lst_Pals, Acum, Ind_mais_1, N, Letras_Comuns).

mesma_letra(_, [], _) :-
    !.

mesma_letra(Ind, [P|R], X) :-
    nth1(Ind, P, El),
    El = X,
    mesma_letra(Ind, R, X).


/* Predicado atribui_comuns/1 */

atribui_comuns([]) :-
    !.

atribui_comuns([P|R]) :-
    nth1(1, P, Espaco),
    nth1(2, P, Lst_Pals),
    letras_comuns(Lst_Pals, Letras_Comuns),
    atribui_espaco(Espaco, Letras_Comuns),
    atribui_comuns(R).

atribui_espaco(_, []) :-
    !.

atribui_espaco(Espaco, [P|R]) :-
    (Num, Letra) = P,
    nth1(Num, Espaco, Letra),
    atribui_espaco(Espaco, R).

/* Predicado retira_impossiveis/2 */

retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis) :-
    retira_impossiveis(Pals_Possiveis, [], Novas_Pals_Possiveis).

retira_impossiveis([], Res, Res) :-
    !.

retira_impossiveis([P|R], Acum, Res) :-
    nth1(1, P, Espaco),
    nth1(2, P, Lst_Pals),
    include(subsumes_term(Espaco), Lst_Pals, Lst_Possiveis),
    append([Espaco], [Lst_Possiveis], Novo_Par),
    append(Acum, [Novo_Par], Novo_Acum),
    retira_impossiveis(R, Novo_Acum, Res).


/* Predicado obtem_unicas/2 */

obtem_unicas(Pals_Possiveis, Unicas) :-
    obtem_unicas(Pals_Possiveis, [], Unicas).

obtem_unicas([], Res, Res) :-
    !.

obtem_unicas([P|R], Acum, Res) :-
    nth1(2, P, Lst_Pals),
    length(Lst_Pals, N),
    N is 1 ->
    append(Acum, Lst_Pals, Novo_Acum),
    obtem_unicas(R, Novo_Acum, Res);
    obtem_unicas(R, Acum, Res).


/* Predicado retira_unicas/2 - Quase terminado*/

retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) :-
    obtem_unicas(Pals_Possiveis, Unicas),
    retira_unicas(Pals_Possiveis, Unicas, [], Novas_Pals_Possiveis).

retira_unicas([], _, Res, Res) :-
    !.

retira_unicas([P|R], Unicas, Acum, Res) :-
    nth1(1, P, Espaco),
    nth1(2, P, Lst_Pals),
    length(Lst_Pals, N),
    N > 1,
    !,
    duplicate_term(Lst_Pals, Lst_Pals_Copia),
    findall(X, (member(X, Lst_Pals_Copia), \+member(X, Unicas)), Lst_Possiveis),
    append([Espaco], [Lst_Possiveis], Novo_Par),
    append(Acum, [Novo_Par], Novo_Acum),
    retira_unicas(R, Unicas, Novo_Acum, Res).

retira_unicas([P|R], Unicas, Acum, Res) :-
    nth1(1, P, Espaco),
    nth1(2, P, Lst_Pals),
    append([Espaco], [Lst_Pals], Novo_Par),
    append(Acum, [Novo_Par], Novo_Acum),
    retira_unicas(R, Unicas, Novo_Acum, Res).


/* Predicado simplifica/2 */

simplifica(Pals_Possiveis, Novas_Pals_Possiveis) :-
    atribui_comuns(Pals_Possiveis),
    retira_impossiveis(Pals_Possiveis, Passo2),
    retira_unicas(Passo2, Passo3),
    duplicate_term(Passo3, Res_Possivel),
    espaco_unifica_palavra(Res_Possivel),
    !,
    Novas_Pals_Possiveis = Res_Possivel.

simplifica(Pals_Possiveis, Novas_Pals_Possiveis) :-
    atribui_comuns(Pals_Possiveis),
    retira_impossiveis(Pals_Possiveis, Passo2),
    retira_unicas(Passo2, Passo3),
    simplifica(Passo3, Novas_Pals_Possiveis).

espaco_unifica_palavra([]) :-
    !.

espaco_unifica_palavra([P|R]) :-
    nth1(1, P, Espaco),
    nth1(2, P, Lst_Pals),
    [Espaco] = Lst_Pals,
    espaco_unifica_palavra(R).

/* Predicado inicializa/2 */

inicializa(Puz, Pals_Possiveis) :-
    nth1(1, Puz, Pals),
    nth1(2, Puz, Grelha),
    obtem_letras_palavras(Pals, Lst_Pals),
    espacos_puzzle(Grelha, Espacos),
    palavras_possiveis(Lst_Pals, Espacos, Pals_Possiveis_Prov),
    simplifica(Pals_Possiveis_Prov, Pals_Possiveis).

/* Predicado escolhe_menos_alternativas\2 */

escolhe_menos_alternativas(Pals_Possiveis, Escolha) :-
    maplist(nth1(2), Pals_Possiveis, Espacos),
    maplist(length, Espacos, Comps),
    exclude(==(1), Comps, Comps_Sem_1),
    comp_minimo(Comps_Sem_1, N),
    escolhe_menos_alternativas(Pals_Possiveis, N, Escolha).

escolhe_menos_alternativas([P|R], N, Escolha) :-
    nth1(2, P, Pals),
    length(Pals, N) ->
    Escolha = P;
    escolhe_menos_alternativas(R, N, Escolha).

comp_minimo([P|R], N) :-
    comp_minimo(R, P, N).

comp_minimo([], C, C).

comp_minimo([P|R], Min, N) :- 
    P < Min ->
    comp_minimo(R, P, N);
    comp_minimo(R, Min, N).


/* Predicado experimenta_pal/3 */

experimenta_pal(Escolha, Pals_Possiveis, Novas_Pals_Possiveis) :-
    nth1(1, Escolha, Esp),
    nth1(2, Escolha, Lst_Pals),
    member(Pal, Lst_Pals),
    Esp = Pal,
    experimenta_pal([Esp, [Pal]], Pals_Possiveis, [], Novas_Pals_Possiveis).

experimenta_pal(_, [], Res, Res) :-
    !.

experimenta_pal([Esp, [Pal]], [P|R], Acum, Res) :-
    nth1(1, P, Espaco),
    Espaco == Pal ->
    append(Acum, [[Esp, [Pal]]], Novo_Acum),
    append(Novo_Acum, R, Acum_Final),
    experimenta_pal([Esp, [Pal]], [], Acum_Final, Res);
    append(Acum, [P], Novo_Acum),
    experimenta_pal([Esp, [Pal]], R, Novo_Acum, Res).

/* Predicado resolve_aux/2 */

resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis) :-
    escolhe_menos_alternativas(Pals_Possiveis, Escolha),
    experimenta_pal(Escolha, Pals_Possiveis, Prov),
    simplifica(Prov, Novas_Pals_Possiveis).

/* Predicado resolve/1 */

resolve(Puz) :-
    nth1(2, Puz, Grelha),
    inicializa(Puz, Pals_Possiveis),
    resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis),
    Puz = [Novas_Pals_Possiveis, Grelha].