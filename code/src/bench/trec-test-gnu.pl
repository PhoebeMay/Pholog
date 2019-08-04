preventLastCallOpt(X).
any(X).
test(0).
test(N) :- any(f(a)), N1 is N - 1, test(N1), preventLastCallOpt(N1).
goal :- test(2500000).
:- initialization(goal).

