open Interp
open ParseTree

let test_case =
"
ll(one,three).
ll(one,two).

ll(two,three).

geq(one,one).

geq(two,one).
geq(two,two).

geq(three,one).
geq(three,two).
geq(three,three).

unif(X,X).

less(null,Z, null).
less(pair(H,T), Bound, Result) :- ll(H,Bound), less(T, Bound, NewRes), unif(Result, pair(H, NewRes)).
less(pair(H,T), Bound, Result) :- geq(H,Bound), less(T, Bound, Result).

geq(null,Z, null).
geq(pair(H,T), Bound, Result) :- geq(H,Bound), geq(T, Bound, NewRes), unif(Result, pair(H, NewRes)).
geq(pair(H,T), Bound, Result) :- ll(H, Bound), geq(T, Bound, Result).

part(pair(H,T), Low, Mid, Up) :- unif(Mid, H), less(T,H, Low), geq(T,H, Up).

join(null,Mid, Up, Result) :- unif(Result, pair(Mid,Up)).
join(pair(H,T),Mid, Up, Result) :- join(T, Mid, Up, NewRes), unif(Result, pair(H,NewRes)).

quick(null, null).
quick(pair(X,null), X).
quick(pair(X, pair(Y, null)), Result) :- geq(Y, X), unif(Result, pair(X, pair(Y, null))).

quick(X, Result) :-
part(X, Low, Mid, Up),
quick(Low, ResLow),
quick(Up, ResUp),
join(ResLow, Mid, ResUp, Result).


?- quick(pair(two,pair(one,null)),X)

"
let () =
  Printf.printf "Interp 4\n";
  print_endline "\n";
  let lexbuf = Lexing.from_string test_case in
  let parseTree = Parser.main Lexer.token lexbuf in
  let result = interpret parseTree in
     Printf.printf "The answer is:\n%s\n" (strAns result)
