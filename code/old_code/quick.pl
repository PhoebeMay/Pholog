less(null,_, null).
less(pair(H,T), Bound, Result) :- H < Bound, less(T, Bound, NewRes), Result = pair(H, NewRes).
less(pair(H,T), Bound, Result) :- H >= Bound, less(T, Bound, Result).

geq(null,_, Result) :- Result = null.
geq(pair(H,T), Bound, Result) :- H >= Bound, geq(T, Bound, NewRes), Result = pair(H, NewRes).
geq(pair(H,T), Bound, Result) :- H < Bound, geq(T, Bound, Result).

part(pair(H,T), Low, Mid, Up) :- Mid = H, less(T,H, Low), geq(T,H, Up).

join(null,Mid, Up, Result) :- print(' join base '), Result = pair(Mid,Up).
join(pair(H,T),Mid, Up, Result) :- join(T, Mid, Up, NewRes), Result = pair(H,NewRes), 
    print('joined'), print(Result).

quick(null, Result) :- Result = null, print(" base ").
quick(pair(X,null), Result) :- Result = X, print(" base ").
quick(pair(X, pair(Y, null)), Result) :- X =< Y, Result = pair(X, pair(Y, null)).
quick(X, Result) :- print(' q1 '),
    part(X, Low, Mid, Up),
    quick(Low, ResLow),
    quick(Up, ResUp),
    join(ResLow, Mid, ResUp, Result).
