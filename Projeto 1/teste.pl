espaco_fila(Fila, Esp) :- espaco_fila(Fila, [], Esp).

espaco_fila([], Acum, Res) :-
    length(Acum, N),
    N >= 3,
    Acum = Res.

espaco_fila([P|R], Acum, Res) :-
    P \== #,
    !,
    append(Acum, [P], Novo_Acum),
    espaco_fila(R, Novo_Acum, Res).

espaco_fila([_|R], Acum, Res) :-
    length(Acum, N),
    N >= 3,
    !,
    espaco_fila([], Acum, Res),
    espaco_fila(R, [], Novo_Res).

espaco_fila([_|R], _, Res) :-
    espaco_fila(R, [], Res).

Letras = [[a,m,e,n,o],[a,t,o],[d,a,o],[d,i,a],[d,r,a,m,a],[m,a,e],[m,a,n,d,e],[s,e,d,e],[s,o,a,r]]

palavras_possiveis_esp(Lst_Letras, Espacos, Esp, Pals_Possiveis) :-
    copia(Espacos, Copia),
    findall(Pal, palavra_possivel_esp(Pal, Esp, Copia, Lst_Letras), Pals_Possiveis).

copia(Esp, Copia) :-
    maplist(copia_el, Esp, Copia).

copia_el(El, El).

palavras_possiveis_esp(Lst_Letras, Espacos, Esp, Pals_Possiveis) :-
    palavras_possiveis_esp(Lst_Letras, Espacos, Esp, [], Pals_Possiveis).

palavras_possiveis_esp([], _, _, Res, Res) :-
    !.

palavras_possiveis_esp([P|R], Lst_Esp, Esp, Acum, Pals_Possiveis) :-
    duplicate_term(Lst_Esp, Copia),
    palavra_possivel_esp(P, Esp, Copia, [P|R]),
    append(Acum, [P], Novo_Acum),
    palavras_possiveis_esp(R, Lst_Esp, Esp, Novo_Acum, Pals_Possiveis).

palavras_possiveis_esp([_|R], Lst_Esp, Esp, Acum, Pals_Possiveis) :-
    palavras_possiveis_esp(R, Lst_Esp, Esp, Acum, Pals_Possiveis).

/*remove_duplicados(X, Y) :-
    remove_duplicados(X, [], Y).

remove_duplicados([], Res, Res) :-
    !.

remove_duplicados([P|R], Acum, Res) :-
    member(P, Acum) ->
    remove_duplicados(R, Acum, Res);
    append(Acum, [P], Novo_Acum),
    remove_duplicados(R, Novo_Acum, Res).*/

/* Predicado letras_comuns/2

letras_comuns(Lst_Pals, Letras_Comuns) :-
    nth1(1, Lst_Pals, X),
    length(X, N),
    letras_comuns(Lst_Pals, [], 1, N, Letras_Comuns).

letras_comuns(_, Res, Ind, N, Res) :-
    Ind > N,
    !.

letras_comuns(Lst_Pals, Acum, Ind, N, Letras_Comuns) :-
    maplist(nth1(Ind, Lst_Pals), Lst_Pals) ->
    Ind_mais_1 is Ind + 1,
    nth1(1, Lst_Pals, X),
    nth1(Ind, X, Letra),
    append(Acum, [(N, Letra)], Novo_Acum),
    letras_comuns(Lst_Pals, Novo_Acum, Ind_mais_1, N, Letras_Comuns);
    letras_comuns(Lst_Pals, Acum, Ind_mais_1, N, Letras_Comuns). */

    P = [[s, P16, P17, P18], [[s, e, d, e], [s, o, a, r]]],
    Unicas = [[d,i,a],[m,a,e],[d,a,o],[m,a,n,d,e],[d,r,a,m,a],[a,m,e,n,o],[s,e,d,e]],