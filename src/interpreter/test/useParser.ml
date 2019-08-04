let test_case = "\nanimal(X) :- cat(X).\ncat(fluffy).\n\n?- animal(fluffy)\n"

let () =
  let lexbuf = Lexing.from_string test_case in
  let _result = Parser.main Lexer.token lexbuf in
  print_string "\n";
  print_string "\n\n"
