open Interp
open ParseTree

let test_case =

"
g(dusk).
h(craig).

f(X,Y) :- g(X), h(Y).

?-f(Y,X)
"

let () =
  let lexbuf = Lexing.from_string test_case in
  let parseTree = Parser.main Lexer.token lexbuf in
  let result = interpret parseTree in
  Printf.printf "The answer is:\n%s\n" (strAns result)
