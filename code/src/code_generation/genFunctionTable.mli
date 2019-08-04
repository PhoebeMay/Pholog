open ParseTree
open Dt
open Core

val genFunctionTable : var program ->
  (functionId, (location clause * int) sexp_list) Base.Hashtbl.t *
  (location clauseBodyVal sexp_list * int)
