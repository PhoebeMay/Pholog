let test_case =  "
animal(X) :- cat(X).
cat(fluffy).

?- animal(fluffy)
"


let () =
  let lexbuf = Lexing.from_string test_case in
    let _result = Parser.main Lexer.token lexbuf in
    print_string("\n"); print_string("\n\n")
