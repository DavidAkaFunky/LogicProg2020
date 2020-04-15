duplica_elementos([], []).
duplica_elementos([P|R1], [P, P|R2]) :- duplica_elementos (R1, R2).