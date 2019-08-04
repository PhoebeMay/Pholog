open Interp
open ParseTree

let test_case =
  "\n\
   ll(one,three).\n\
   ll(one,two).\n\n\
   ll(two,three).\n\n\
   geq(one,one).\n\n\
   geq(two,one).\n\
   geq(two,two).\n\n\
   geq(three,one).\n\
   geq(three,two).\n\
   geq(three,three).\n\n\
   unif(X,X).\n\n\
   less(null,Z, null).\n\
   less(pair(H,T), Bound, Result) :- ll(H,Bound), less(T, Bound, NewRes), \
   unif(Result, pair(H, NewRes)).\n\
   less(pair(H,T), Bound, Result) :- geq(H,Bound), less(T, Bound, Result).\n\n\
   geq(null,Z, null).\n\
   geq(pair(H,T), Bound, Result) :- geq(H,Bound), geq(T, Bound, NewRes), \
   unif(Result, pair(H, NewRes)).\n\
   geq(pair(H,T), Bound, Result) :- ll(H, Bound), geq(T, Bound, Result).\n\n\
   part(pair(H,T), Low, Mid, Up) :- unif(Mid, H), less(T,H, Low), geq(T,H, Up).\n\n\
   join(null,Mid, Up, Result) :- unif(Result, pair(Mid,Up)).\n\
   join(pair(H,T),Mid, Up, Result) :- join(T, Mid, Up, NewRes), unif(Result, \
   pair(H,NewRes)).\n\n\
   quick(null, null).\n\
   quick(pair(X,null), X).\n\
   quick(pair(X, pair(Y, null)), Result) :- geq(Y, X), unif(Result, pair(X, \
   pair(Y, null))).\n\n\
   quick(X, Result) :-\n\
   part(X, Low, Mid, Up),\n\
   quick(Low, ResLow),\n\
   quick(Up, ResUp),\n\
   join(ResLow, Mid, ResUp, Result).\n\n\n\
   ?- quick(pair(two,pair(one,null)),X)\n\n"

let () =
  Printf.printf "Machine 1\n" ;
  print_endline "\n" ;
  let lexbuf = Lexing.from_string test_case in
  let parseTree = Parser.main Lexer.token lexbuf in
  let result = interpret parseTree in
  Printf.printf "The answer is:\n%s\n" (strAns result)
