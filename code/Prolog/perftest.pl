h(Z).

build(null,0).
build(cons(X),N) :- N1 is N + -1, build(X,N1), h(X).

run :-  build(X,1000000), halt.

