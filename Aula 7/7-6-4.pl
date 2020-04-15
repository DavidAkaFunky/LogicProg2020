nao_membro(_, []).
nao_membro(El, [P|R]) :- El \== P, !, nao_membro(El, R).
insere_ordenado (El, [], [El]).
insere_ordenado (El, [P|R], [El, P|R]) :- El =< P.
insere_ordenado (El, [P|R1], [P|R2]) :- El > P, insere_ordenado (El, R1, R2).
junta_novo_aleatorio(L1, Lim_Inf, Lim_Sup, L2) :- Lim_Inf =< Lim_Sup, random_between(Lim_Inf, Lim_Sup, N), nao_membro(N, L1), insere_ordenado (N, L1, L2).
n_aleatorios(N, Inf, Sup, Lst) :- n_aleatorios(N, Inf, Sup, Lst, []).
n_aleatorios(N, _, _, Lst, Lst) :- length(Lst, N).
n_aleatorios(N, Inf, Sup, Lst, Aux) :- junta_novo_aleatorio(Aux, Inf, Sup, Aux_Novo), n_aleatorios(N, Inf, Sup, Lst, Aux_Novo).