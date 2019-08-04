move(1,X,Y,_,1).


move(N,X,Y,Z,Count) :-
    M is N-1,
    move(M,X,Z,Y,C1),
    move(1,X,Y,_,C2),
    move(M,Z,Y,X,C3),
    Count is C1+C2+C3.


goal :- move(19,left,right,center,R).


:- initialization(goal).
