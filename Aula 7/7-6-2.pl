insere_ordenado (El, [], [El]).
insere_ordenado (El, [P|R], [El, P|R]) :- El =< P.
insere_ordenado (El, [P|R1], [P|R2]) :- El > P, insere_ordenado (El, R1, R2).