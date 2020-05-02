/********************************************************************************/                                                 
/*                            Ficheiro: Projeto1.pl                             */
/*      Nome: David Emanuel Silva Belchior -- Instituto Superior Tecnico        */
/*    Curso: LEIC-A -- Numero: 95550 -- Disciplina: Logica para Programacao     */
/*     Descricao: Criacao de um programa de resolucao de palavras cruzadas,     */
/*             com diversos predicados destinados a esse proposito.             */
/********************************************************************************/


/********************************************************************************/
/*                 PARTE 0 - CARREGAMENTO DE FICHEIRO AUXILIAR                  */
/*   O ficheiro codigo_comum.pl e usado para abstracao de alguns predicados.    */
/********************************************************************************/

:- [codigo_comum].

/********************************************************************************/
/*                     PARTE 1 - INICIALIZACAO DE PUZZLES                       */
/********************************************************************************/

/********************************************************************************/
/*                   1.1 - Predicado obtem_letras_palavras/2                    */
/*                                                                              */
/*       No predicado obtem_letras_palavras(Lst_Pals, Letras), Lst_Pals e       */
/*      uma lista de palavras fornecida e Letras corresponde a uma lista de     */
/*    listas de chars (cada uma correspondente a uma das palavras originais).   */
/********************************************************************************/

obtem_letras_palavras(Lst_Pals, Letras) :-
    sort(Lst_Pals, Lst_Ord),
    obtem_ja_ordenado(Lst_Ord, Letras). 

/* Predicado auxiliar obtem_ja_ordenado/2 - No predicado obtem_ja_ordenado(Lst_Ord, Letras),
Letras e o resultado de separar, letra a letra, cada palavra de Lst_Ord. */

obtem_ja_ordenado([], []).

obtem_ja_ordenado([P1|R1], [P2|R2]) :-
    atom_chars(P1, P2),
    obtem_ja_ordenado(R1, R2).


/********************************************************************************/
/*                        1.2 - Predicado espaco_fila/2                         */
/*                                                                              */
/*     No predicado espaco_fila(Fila, Esp), Fila e uma fila fornecida e Esp     */
/*    e um espaco pertencente a Fila, se existir como definido no enunciado.    */
/********************************************************************************/

espaco_fila(Fila, Esp) :- 
    lista_espacos(Fila, L_Esp),
    member(Esp, L_Esp).

/* Predicado lista_espacos/2 - No predicado auxiliar lista_espacos(Fila, L_Esp),
L_Esp corresponde a lista completa de espacos de Fila. */

lista_espacos(Fila, L_Esp) :- 
    lista_espacos(Fila, [], [], L_Esp).

lista_espacos([], [], Res, Res) :-
    !.

/* Caso pseudo-terminal: O acumulador dos espacos (Acum) e enviado
para L (lista dos espacos), e e executado o caso terminal. */
lista_espacos([], Acum, L, Res) :-
    length(Acum, N),
    N >= 3 ->
    append(L, [Acum], Nova_L),
    lista_espacos([], [], Nova_L, Res);
    lista_espacos([], [], L, Res).

/* Se P for #, Acum e adicionado a L e e reiniciado, caso contrario, P e adicionado a Acum. */
lista_espacos([P|R], Acum, L, Res) :-
    P == # ->
    (length(Acum, N),
    N >= 3 ->
    append(L, [Acum], Nova_L),
    lista_espacos(R, [], Nova_L, Res);
    lista_espacos(R, [], L, Res));
    append(Acum, [P], Novo_Acum),
    lista_espacos(R, Novo_Acum, L, Res).

/********************************************************************************/
/*                       1.3 - Predicado espacos_fila/2                         */
/*                                                                              */
/*            No predicado espacos_fila(Fila, Esp), Fila e uma fila             */
/*              fornecida e Esp e a lista dos espacos pertencente               */
/*               a Fila, se existirem como definido no enunciado.               */
/********************************************************************************/


espacos_fila(Fila, []) :-
    \+espaco_fila(Fila, _),
    !.

espacos_fila(Fila, Espacos) :- 
    bagof(X, espaco_fila(Fila, X), Espacos).


/********************************************************************************/
/*                      1.4 - Predicado espacos_puzzle/2                        */
/*                                                                              */
/*      No predicado espacos_puzzle(Grelha, Espacos), Grelha e uma grelha       */
/*  fornecida, e Espacos e a lista dos espacos que podem ser obtidos da grelha  */
/*        original (por linhas) e da grelha transposta (por colunas).           */
/********************************************************************************/


espacos_puzzle(Grelha, Espacos) :-
    espacos_linhas(Grelha, Linhas),
    mat_transposta(Grelha, Grelha_Tposta),
    espacos_linhas(Grelha_Tposta, Colunas),
    append(Linhas, Colunas, Espacos).


/* Predicado auxiliar espacos_linhas/2 - No predicado espacos_linhas(Grelha, Espacos), 
Grelha e a grelha fornecida no predicado espacos_puzzle/2 segundo uma orientacao 
(por linhas ou por colunas), e Espacos e o conjunto de espacos correspondentes a Grelha 
nessa orientacao. */

espacos_linhas(Grelha, Espacos) :- 
    espacos_linhas(Grelha, [], Espacos).

espacos_linhas([], Res, Res) :-
    !.

espacos_linhas([P|R], Aux, Res) :-
    espacos_fila(P, Esp),
    Esp \== [] ->
    append(Aux, Esp, Novo_Aux),
    espacos_linhas(R, Novo_Aux, Res);
    espacos_linhas(R, Aux, Res).


/********************************************************************************/
/*                1.5 - Predicado espacos_com_posicoes_comuns/3                 */
/*                                                                              */
/*   No predicado espacos_com_posicoes_comuns(Espacos, Esp, Esps_Com), Espacos  */
/*   e uma lista de espacos fornecida, Esp e um espaco fornecido pertencente a  */
/*  Espacos,e Esps_Com e a lista dos espacos com posicoes comuns a esse espaco. */
/********************************************************************************/


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

/* Predicado intersecao/3 - No predicado auxiliar intersecao(L1, L2, Res), 
Res corresponde a intersecao de L1, L2, os quais sao fornecidos.
   Esta versao e diferente do predicado intersection/3, pois uma entidade 
so e adicionada a intersecao se for estritamente igual em L1 e L2, nao sendo
adicionada se for apenas possivel unifica-la com outra. */


intersecao(L1, L2, Res) :-
    intersecao(L1, L2, [], Res).

intersecao([], _, Res, Res) :-
    !.

intersecao(_, [], Res, Res) :-
    !.

intersecao([P1|R1], L2, Lst, Res) :-
    membro_var(P1, L2) ->
    append(Lst, [P1], Nova_Lst),
    intersecao(R1, L2, Nova_Lst, Res);
    intersecao(R1, L2, Lst, Res).

/* Predicado membro_var/2 - Diferente de membro/2 ou member/2, X so e membro de [P|R]
se existir um elemento que seja estritamente igual a ele, e nao apenas unificavel. */


membro_var(X, [P|R]) :-
    X == P;
    membro_var(X, R).


/*******************************************************************************/
/*                    1.6 - Predicado palavra_possivel_esp/4                   */
/*                                                                             */
/*      No predicado palavra_possivel_esp(Pal, Esp, Espacos, Lst_Letras),      */
/*    Pal e uma palavra (como lista de letras) que pode ser colocada em Esp,   */
/*                 o qual e um espaco pertencente a Espacos,                   */
/*   uma lista de espacos fornecida, e Letras e uma lista de listas de letras. */
/*******************************************************************************/

palavra_possivel_esp(Pal, Esp, Espacos, Letras) :-
    espacos_com_posicoes_comuns(Espacos, Esp, Lst_Com),
    Esp = Pal,
    duplicate_term(Lst_Com, Copia),
    correto_comuns(Copia, Letras).

/* Predicado auxiliar correto_comuns/2 - No predicado auxiliar correto_comuns(Copia, Letras),
e colocado em Copia (copia da lista dos espacos comuns de Esp a Espacos, Lst_Com) as palavras
de Letras. Se Copia for totalmente preenchida, significa que Pal e uma palavra possivel para
o espaco Esp. */

correto_comuns([], _) :-
    !.
    
correto_comuns([P|R], Lst) :-
    member(P, Lst),
    correto_comuns(R, Lst),
    !.


/********************************************************************************/
/*                   1.7 - Predicado palavras_possiveis_esp/4                   */
/*                                                                              */
/*No predicado palavras_possiveis_esp(Lst_Letras, Espacos, Esp, Pals_Possiveis),*/
/*       Pals_Possiveis e a lista ordenada de palavras para o espaco Esp,       */
/*                  tendo em conta a lista de espacos, Espacos,                 */
/*                  e a lista de letras, Lst_Letras, fornecidas.                */
/********************************************************************************/

palavras_possiveis_esp(Lst_Letras, Espacos, Esp, Pals_Possiveis) :-
    findall(Pal,
            (member(Pal, Lst_Letras), palavra_possivel_esp(Pal, Esp, Espacos, Lst_Letras)),
            Pals_Possiveis).


/********************************************************************************/
/*                     1.8 - Predicado palavras_possiveis/3                     */
/*                                                                              */
/*       No predicado palavras_possiveis(Letras, Espacos, Pals_Possiveis),      */     
/*             Pals_Possiveis e a lista ordenada de palavras para a             */
/*   lista de espacos, Espacos, tendo em conta a lista de letras, Lst_Letras.   */
/*         Pals_Possiveis e composto por uma lista de pares de listas,          */
/*        cada uma contendo um espaco e as palavras possiveis para ele.         */
/********************************************************************************/

palavras_possiveis(Letras, Espacos, Pals_Possiveis) :-
    palavras_possiveis(Letras, Espacos, Espacos, [], Pals_Possiveis),
    !.

palavras_possiveis(_, _, [], Res, Res).

palavras_possiveis(Letras, Espacos, [P|R], Aux, Res) :-
    palavras_possiveis_esp(Letras, Espacos, P, Poss_Esp),
    append(Aux, [[P,Poss_Esp]], Novo_Aux),
    palavras_possiveis(Letras, Espacos, R, Novo_Aux, Res).

/********************************************************************************/
/*                        1.9 - Predicado letras_comuns/2                       */
/*                                                                              */
/*      No predicado letras_comuns(Lst_Pals, Letras_Comuns), Letras_Comuns      */
/*   indica quais as posicoes de Lst_Palavras com a mesma letra, e qual e ela.  */
/*         O resultado de cada par e expresso da forma (Posicao, Letra).        */
/********************************************************************************/

/* O predicado e realizado N vezes, correspondendo ao numero de letras de cada palavra de Lst_Pals. */
letras_comuns(Lst_Pals, Letras_Comuns) :-
    nth1(1, Lst_Pals, El),
    length(El, N),
    letras_comuns(Lst_Pals, [], 1, N, Letras_Comuns).


letras_comuns(_, Res, Ind, N, Res) :-
    Ind > N,
    !.

/* Se a letra indice Ind de cada palavra de Lst_Pals for igual, X sera essa letra. */
letras_comuns(Lst_Pals, Acum, Ind, N, Letras_Comuns) :-
    mesma_letra(Ind, Lst_Pals, X) ->
    append(Acum, [(Ind, X)], Novo_Acum),
    Ind_mais_1 is Ind+1,
    letras_comuns(Lst_Pals, Novo_Acum, Ind_mais_1, N, Letras_Comuns);
    Ind_mais_1 is Ind+1,
    letras_comuns(Lst_Pals, Acum, Ind_mais_1, N, Letras_Comuns).


/* Predicado uxiliar mesma_letra/2 - No predicado mesma_letra(Ind, Lst_Pals, X), X corresponde
a letra do indice Ind de cada palavra de Lst_Pals, se essa letra for sempre a mesma. */

mesma_letra(_, [], _) :-
    !.

mesma_letra(Ind, [P|R], X) :-
    nth1(Ind, P, El),
    El = X,
    mesma_letra(Ind, R, X).


/********************************************************************************/
/*                       1.10 - Predicado atribui_comuns/1                      */
/*                                                                              */
/*           No predicado atribui_comuns(Lst_Pals), sao substituidas            */
/*           as entradas de cada espaco as quais correspondem letras            */
/*              sempre iguais, como obtido no predicado anterior.               */
/********************************************************************************/

atribui_comuns([]) :-
    !.

atribui_comuns([P|R]) :-
    nth1(1, P, Espaco),
    nth1(2, P, Lst_Pals),
    letras_comuns(Lst_Pals, Letras_Comuns),
    atribui_espaco(Espaco, Letras_Comuns),
    atribui_comuns(R).


/* Predicado atribui_espaco/2 - No predicado auxiliar atribui_espaco(Espaco, Letras_Comuns),
e atribuido a Espaco, por unificacao, as Letras_Comuns obtidas no predicado letras_comuns/2. */

atribui_espaco(_, []) :-
    !.

atribui_espaco(Espaco, [P|R]) :-
    (Num, Letra) = P,
    nth1(Num, Espaco, Letra),
    atribui_espaco(Espaco, R).

/********************************************************************************/
/*                     1.11 - Predicado retira_impossiveis/2                    */
/*                                                                              */
/*    No predicado retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis),    */
/*   Novas_Pals_Possiveis e o resultado de modificar, de forma nao destrutiva,  */
/*    Pals_Possiveis, nao incluindo as palavras que nao podem pertencer a um    */
/*    dado espaco, devido as alteracoes feitas no predicado atribui_comuns/2.   */
/********************************************************************************/

retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis) :-
    retira_impossiveis(Pals_Possiveis, [], Novas_Pals_Possiveis).

retira_impossiveis([], Res, Res) :-
    !.

retira_impossiveis([P|R], Acum, Res) :-
    nth1(1, P, Espaco),
    nth1(2, P, Lst_Pals),
    include(subsumes_term(Espaco), Lst_Pals, Lst_Possiveis),
    append(Acum, [[Espaco, Lst_Possiveis]], Novo_Acum),
    retira_impossiveis(R, Novo_Acum, Res).


/********************************************************************************/
/*                        1.12 - Predicado obtem_unicas/2                       */
/*                                                                              */
/*              No predicado obtem_unicas(Pals_Possiveis, Unicas),              */
/*            Unicas e a lista com os as palavras de Pals_Possiveis             */
/*                que sao a unica possibilidade para um espaco.                 */
/********************************************************************************/

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


/********************************************************************************/
/*                        1.13 - Predicado retira_unicas/2                      */
/*                                                                              */
/*    No predicado retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis),    */
/*        Novas_Pals_Possiveis e o resultado de modificar, de forma nao         */
/*        destrutiva, Pals_Possiveis, nao incluindo as palavras que sao         */
/*       unicas para outro espaco que nao o que esta a ser inspecionado.        */
/********************************************************************************/

retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) :-
    obtem_unicas(Pals_Possiveis, Unicas),
    retira_unicas(Pals_Possiveis, Unicas, [], Novas_Pals_Possiveis).

retira_unicas([], _, Res, Res) :-
    !.

/* Caso mais que uma palavra esteja dada como possivel para o espaco P, nao sao
incluidas as que ja sao unicas para outro espaco de Pals_Possiveis. */
retira_unicas([P|R], Unicas, Acum, Res) :-
    nth1(1, P, Espaco),
    nth1(2, P, Lst_Pals),
    length(Lst_Pals, N),
    N > 1,
    !,
    duplicate_term(Lst_Pals, Lst_Pals_Copia),
    findall(X, (member(X, Lst_Pals_Copia), \+member(X, Unicas)), Lst_Possiveis),
    append(Acum, [[Espaco, Lst_Possiveis]], Novo_Acum),
    retira_unicas(R, Unicas, Novo_Acum, Res).

/* Caso contrario, P e adicionado a lista-resultado sem restricoes. */
retira_unicas([P|R], Unicas, Acum, Res) :-
    append(Acum, [P], Novo_Acum),
    retira_unicas(R, Unicas, Novo_Acum, Res).


/********************************************************************************/
/*                         1.14 - Predicado simplifica/2                        */
/*                                                                              */
/*        No predicado simplifica(Pals_Possiveis, Novas_Pals_Possiveis),        */
/*         Novas_Pals_Possiveis e o resultado de aplicar os predicados          */
/*         atribui_comuns/2, retira_impossiveis/2 e retira_unicas/2 a           */
/*              Pals_Possiveis ate nao ser possivel realizar mais               */
/*               alteracoes, de modo a simplificar a lista dada.                */
/********************************************************************************/

simplifica(Pals_Possiveis, Novas_Pals_Possiveis) :-
    simplifica(Pals_Possiveis, Pals_Possiveis, Novas_Pals_Possiveis).

/* Se a lista obtida no passo anterior (ou Pals_Possiveis, no primeiro passo) for igual a
lista resultante dos passos obtidos (Passo3), Novas_Pals_Possiveis unificara com essa lista. */
simplifica(Temp, Pals_Possiveis, Novas_Pals_Possiveis) :-
    atribui_comuns(Pals_Possiveis),
    retira_impossiveis(Pals_Possiveis, Passo2),
    retira_unicas(Passo2, Passo3),
    Passo3 == Temp,
    !,
    Novas_Pals_Possiveis = Passo3.

/* Caso contrario, o algoritmo e repetido. */
simplifica(_, Pals_Possiveis, Novas_Pals_Possiveis) :-
    atribui_comuns(Pals_Possiveis),
    retira_impossiveis(Pals_Possiveis, Passo2),
    retira_unicas(Passo2, Passo3),
    simplifica(Pals_Possiveis, Passo3, Novas_Pals_Possiveis).

/********************************************************************************/
/*                         1.15 - Predicado inicializa/2                        */
/*                                                                              */
/*   No predicado inicializa(Puz, Novas_Pals_Possiveis), sao efetuados a Puz    */
/*    dividido numa lista de palavras, Pals, e numa Grelha), os predicados      */
/*      obtem_letras_palavras/2, espacos_puzzle/2, palavras_possiveis/3 e       */
/*     simplifica/2, por esta ordem, de modo a obter uma lista de palavras      */
/*                possiveis para cada espaco, Pals_Possiveis.                   */
/********************************************************************************/

inicializa(Puz, Pals_Possiveis) :-
    nth1(1, Puz, Pals),
    nth1(2, Puz, Grelha),
    obtem_letras_palavras(Pals, Lst_Pals),
    espacos_puzzle(Grelha, Espacos),
    palavras_possiveis(Lst_Pals, Espacos, Pals_Possiveis_Prov),
    simplifica(Pals_Possiveis_Prov, Pals_Possiveis).


/********************************************************************************/
/*             PARTE 2 - RESOLUCAO DE LISTAS DE PALAVRAS POSSIVEIS              */
/********************************************************************************/

/********************************************************************************/
/*                  2.1 - Predicado escolhe_menos_alternativas/2                */
/*                                                                              */
/*   No predicado escolhe_menos_alternativas(Pals_Possiveis, Escolha), Escolha  */
/*   corresponde ao primeiro elemento de Pals_Possiveis cuja lista de palavras  */
/*     possiveis possui o menor comprimento superior a 1, se alguma existir.    */
/********************************************************************************/

escolhe_menos_alternativas(Pals_Possiveis, Escolha) :-
    maplist(nth1(2), Pals_Possiveis, Palavras),
    maplist(length, Palavras, Comps),
    exclude(==(1), Comps, Comps_Sem_1),
    comp_minimo(Comps_Sem_1, N),
    escolhe_menos_alternativas(Pals_Possiveis, N, Escolha).

escolhe_menos_alternativas([P|R], N, Escolha) :-
    nth1(2, P, Pals),
    length(Pals, N) ->
    Escolha = P;
    escolhe_menos_alternativas(R, N, Escolha).

/* Predicado auxiliar comp_minimo/2 - No predicado comp_minimo(Comps_Sem_1, N),
Comps_Sem_1 corresponde a lista dos comprimentos das listas de palavras
possiveis de Pals_Possiveis, excluindo as que possuem comprimento 1. */

comp_minimo([P|R], N) :-
    comp_minimo(R, P, N).

comp_minimo([], C, C).

comp_minimo([P|R], Min, N) :- 
    P < Min ->
    comp_minimo(R, P, N);
    comp_minimo(R, Min, N).


/********************************************************************************/
/*                       2.2 - Predicado experimenta_pal/3                      */
/*                                                                              */
/* No predicado experimenta_pal(Escolha, Pals_Possiveis, Novas_Pals_Possiveis), */
/*          Novas_Pals_Possiveis e o resultado de substituir, de forma          */
/*       nao destrutiva, um dado par espaco/palavras de Pals_Possiveis por      */
/*           [Esp, [Pal]], onde Esp corresponde ao espaco substituido,          */
/*    unificado com Pal, que e uma palavra inicialmente existente naquele par   */
/*                    (Esp e Pal pertencem ambos a Escolha).                    */
/********************************************************************************/

experimenta_pal(Escolha, Pals_Possiveis, Novas_Pals_Possiveis) :-
    nth1(1, Escolha, Esp),
    nth1(2, Escolha, Lst_Pals),
    member(Pal, Lst_Pals),
    Esp = Pal,
    experimenta_pal([Esp, [Pal]], Pals_Possiveis, [], Novas_Pals_Possiveis).

experimenta_pal(_, [], Res, Res) :-
    !.

/* Se, apos unificacao (devido ao passo Esp = Pal), Espaco for estritamente igual a Pal,
tal significa que a o par espaco/palavra a substituir for encontrado, e o algoritmo termina
apos colocacao do resto da lista (R) no acumulador (que correspondera a lista-resultado).
Caso contrario, P e colocado, e o algoritmo e repetido ate o primeiro caso ocorrer.      */
experimenta_pal([Esp, [Pal]], [P|R], Acum, Res) :-
    nth1(1, P, Espaco),
    Espaco == Pal ->
    append(Acum, [[Esp, [Pal]]], Novo_Acum),
    append(Novo_Acum, R, Acum_Final),
    experimenta_pal([Esp, [Pal]], [], Acum_Final, Res);
    append(Acum, [P], Novo_Acum),
    experimenta_pal([Esp, [Pal]], R, Novo_Acum, Res).

/********************************************************************************/
/*                         2.3 - Predicado resolve_aux/2                        */
/*                                                                              */
/*        No predicado resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis),       */
/*       Novas_Pals_Possiveis e o resultado de aplicar, a Pals_Possiveis,       */
/*       os predicados escolhe_menos_alternativas/2, experimenta_pal/2 e        */
/*   simplifica/2, ate que a lista obtida contenha apenas listas de palavras    */
/* possiveis unitarias (e, portanto, escolhe_menos_alternativas devolve false.),*/
/*  realizando-se simplifica/2 uma ultima vez apos essa condicao se verificar.  */
/********************************************************************************/

resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis) :-
    \+escolhe_menos_alternativas(Pals_Possiveis, _),
    !,
    simplifica(Pals_Possiveis, Novas_Pals_Possiveis).

resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis) :-
    escolhe_menos_alternativas(Pals_Possiveis, Escolha),
    experimenta_pal(Escolha, Pals_Possiveis, Passo1),
    simplifica(Passo1, Passo2),
    resolve_aux(Passo2, Novas_Pals_Possiveis).

/********************************************************************************/
/*                        PARTE 3 - RESOLUCAO DE PUZZLES                        */
/*                                                                              */
/*              Esta parte contem apenas um predicado, resolve/1.               */
/*        No predicado resolve(Puz), sao efetuados em Puz os predicados         */
/*   inicializa/1 e resolve_aux/2, de forma a resolver o puzzle por completo.   */
/********************************************************************************/

resolve(Puz) :-
    inicializa(Puz, Pals_Possiveis),
    resolve_aux(Pals_Possiveis, _).