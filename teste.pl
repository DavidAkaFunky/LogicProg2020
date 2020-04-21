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
