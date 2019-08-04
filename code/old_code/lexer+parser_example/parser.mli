type token =
  | INT of (int)
  | PLUS
  | MINUS
  | END

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ParseTree.parseTree
