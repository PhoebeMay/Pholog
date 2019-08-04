iter(0).

iter(N) :- N1 is N - 1, iter(N1).

goal :- iter(100000000).


:- initialization(goal).
