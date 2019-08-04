less(null,_, null).
less(pair(H,T), Bound, Result) :- H < Bound, less(T, Bound, NewRes), Result = pair(H, NewRes).
less(pair(H,T), Bound, Result) :- H >= Bound, less(T, Bound, Result).

geq(null,_, null).
geq(pair(H,T), Bound, Result) :- H >= Bound, geq(T, Bound, NewRes), Result = pair(H, NewRes).
geq(pair(H,T), Bound, Result) :- H < Bound, geq(T, Bound, Result).

part(pair(H,T), Low, Mid, Up) :- Mid = H, less(T,H, Low), geq(T,H, Up).

join(null,Mid, Up, Result) :- Result = pair(Mid,Up).
join(pair(H,T),Mid, Up, Result) :- join(T, Mid, Up, NewRes), Result = pair(H,NewRes).

quick(null, null).
quick(pair(X,null), X).
quick(pair(X, pair(Y, null)), Result) :- X =< Y, Result = pair(X, pair(Y, null)).
quick(X, Result) :- 
    part(X, Low, Mid, Up),
    quick(Low, ResLow),
    quick(Up, ResUp),
    join(ResLow, Mid, ResUp, Result).

