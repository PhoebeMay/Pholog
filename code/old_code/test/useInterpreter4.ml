open Interp
open ParseTree

let test_case =
"
ll(one,three).
ll(one,two).

ll(two,three).

gg(one,one).

gg(two,one).
gg(two,two).

gg(three,one).
gg(three,two).
gg(three,three).

unif(X,X).

less(null,Z, null).

less(pair(H,T), Bound, Result) :- ll(H,Bound), less(T, Bound, NewRes), unif(Result, pair(H, NewRes)).

less(pair(H,T), Bound, Result) :- geq(H,Bound), less(T, Bound, Result).

geq(null,Z, null).

geq(pair(H,T), Bound, Result) :- gg(H,Bound), geq(T, Bound, NewRes), unif(Result, pair(H, NewRes)).

geq(pair(H,T), Bound, Result) :- ll(H, Bound), geq(T, Bound, Result).

part(pair(H,T), Low, Mid, Up) :- unif(Mid, H), less(T,H, Low), geq(T,H, Up).


?- part(pair(two,pair(one,null)),X,B,C)

"

let () =
  Printf.printf "Interp 4\n";
  print_endline "\n";
  let lexbuf = Lexing.from_string test_case in
  let parseTree = Parser.main Lexer.token lexbuf in
  let result = interpret parseTree in
     Printf.printf "The answer is:\n%s\n" (strAns result)
