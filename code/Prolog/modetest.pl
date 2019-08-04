%!  perm(+List, -Perm).


take([H|T],H,T).
take([H|T],A,[H|B]) :- take(T,A,B).



perm([],[]).
perm(L,[H|T]) :- take(L,H,R), perm(R,T).

?- perm([1,2,3],X), print(X).


f