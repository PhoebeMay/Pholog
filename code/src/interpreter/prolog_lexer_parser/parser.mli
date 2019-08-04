
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | RBRAC
  | QUES
  | NAME of (string)
  | LBRAC
  | FSTOP
  | END
  | COMMA
  | ARR

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (string ParseTree.program)
