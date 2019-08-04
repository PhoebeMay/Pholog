open Interp
open ParseTree

let test_case =
  "
f(s(X)).

g(s(Y)).

?- f(X), g(X)
"


let () =
  let lexbuf = Lexing.from_string test_case in
  let parseTree = Parser.main Lexer.token lexbuf in
  let result = interpret parseTree in
  Printf.printf "The answer is:\n%s\n" (strAns result)
