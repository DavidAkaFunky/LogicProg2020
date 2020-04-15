comp_maior_lista ([], C, C).
comp_maior_lista ([P|R], N) :- length (P, Max), comp_maior_lista (R, Max, N).
comp_maior_lista ([P|R], Max, N) :- length (P, X), X >= Max, comp_maior_lista ([P|R], X, N).
comp_maior_lista ([P|R], Max, N) :- length (P, X), X < Max, comp_maior_lista ([P|R], Max, N).