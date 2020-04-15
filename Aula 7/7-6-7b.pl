duplica_elementos(L1, L2) :- duplica_elementos(L1, [], L2).
duplica_elementos([], Aux, Aux).
duplica_elementos([P|R], Aux, L2) :- duplica_elementos(R, [Aux|P, P], L2).