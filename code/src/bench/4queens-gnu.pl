take(c(H,T),H,T).
take(c(H,T),R,c(H,S)) :- take(T,R,S).

perm(eol,eol).
perm(X,c(H,T)) :- take(X,H,R), perm(R,T).

equals(X, X).
notequals(X, Y) :- equals(X, Y), !, fail.
notequals(X, Y).

checkPair(X,Y,Dist) :-
    X1 is X + Dist,
    X2 is X - Dist,
notequals(Y, X1),
    notequals(Y,X2).

checkHead(c(X,eol), Y).


checkHead(c(Q1,c(Q2,T)), Dist) :-
Dist2 is Dist + 1,
checkPair(Q1,Q2, Dist2),
checkHead(c(Q1,T), Dist2).

checkDiags(c(X,eol)).
checkDiags(c(Q1,T)) :- checkHead(c(Q1,T),0), checkDiags(T).


goal :- perm(c(1,c(2,c(3,c(4,eol)))),R), checkDiags(R),halt.



:- initialization(goal).
