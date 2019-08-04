type list A = c(A, list(A)), eol.

pred leq(int,int).
leq(1,1).
leq(1,2).
leq(1,3).
leq(2,2).
leq(2,3).
leq(3,3).

pred take(list(A),A,list(A)).
take(c(H,T),H,T).
take(c(H,T),R,c(H,S)) :- take(T,R,S).

pred perm(list(A),list(A)).
perm(eol,eol).
perm(X,c(H,T)) :- take(X,H,R), perm(R,T).

pred sorted(list(int)).
sorted(eol).
sorted(c(H,eol)).
sorted(c(H1,c(H2,T))) :- leq(H1,H2), sorted(c(H2,T)).

pred terribleSort(list(int),list(int)).
terribleSort(X,Y) :- perm(X,Y), sorted(Y).


?- terribleSort(c(2,c(3,c(1,c(2,eol)))),A)


