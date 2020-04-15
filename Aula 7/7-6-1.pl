nao_membro(_, []).
nao_membro(El, [P|R]) :- El \== P, !, nao_membro(El, R).