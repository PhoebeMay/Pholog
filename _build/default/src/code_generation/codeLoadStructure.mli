open ParseTree
open Dt
open Core

val loadAllFunBody :
     location term sexp_list
  -> location Hash_set.t
  -> int ref
  -> structMapGen
  -> instruction list * instruction list
