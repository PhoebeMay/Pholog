
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | TYPEDEF
  | RBRAC
  | QUES
  | PRED
  | PLUS
  | NAME of (string)
  | MINUS
  | LBRAC
  | IS
  | INTTYP
  | INT of (int)
  | FSTOP
  | FAIL
  | EQUALS
  | END
  | CUT
  | COMMA
  | ARR

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (string ParseTree.program)
