
a(0,Y,Res) :-  Res is Y+1.
a(X,0,Res) :-  X1 is X-1, a(X1,1,Res).
a(X,Y,Res) :-  X1 is X-1, Y1 is Y-1, a(X,Y1,Res1), a(X1,Res1,Res).


goal :- sleep(0.13), a(3,8,R),halt.


:- initialization(goal).