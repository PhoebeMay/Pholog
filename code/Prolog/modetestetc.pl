last([H],H).
last([_|T],H) :- last(T,H).


:- 
last([1,2,3],A),
print(A).