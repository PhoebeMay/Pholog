(* The type of tokens. *)

type token =
  | VAR of string
  | TYPEDEF
  | RBRAC
  | QUES
  | PRED
  | PLUS
  | NAME of string
  | MINUS
  | LBRAC
  | IS
  | INTTYP
  | INT of int
  | FSTOP
  | FAIL
  | EQUALS
  | END
  | CUT
  | COMMA
  | ARR

exception (* This exception is raised by the monolithic API functions. *)
            Error

(* The monolithic API. *)

val main :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> string ParseTree.program
