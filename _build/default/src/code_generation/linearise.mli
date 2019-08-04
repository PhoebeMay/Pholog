open Core
open Dt

val lineariseCode :
     (functionId * (int * instruction sexp_list) sexp_list) sexp_list
  -> instruction sexp_array
(*
val lineariseCode : (functionId * (int * instruction sexp_list) sexp_list) sexp_list ->
(functionId * int, int) Base.Hashtbl.t -> instruction sexp_array *)
