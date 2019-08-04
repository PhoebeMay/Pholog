open Core

(* module Phoebe : Base__.Hash_set_intf.Key =
   type t  *)
type phoebeint = P of int [@@deriving sexp, show]

let hi =
  (module struct
    type t = phoebeint
    let compare (P(x)) (P(y)) = 0
    let sexp_of_t = sexp_of_phoebeint
    let hash (P(x)) = x
  end : Base__.Hash_set_intf.Key with type t = phoebeint)


let b = Base.Hash_set.create hi
let d = Base.Hash_set.add b (P(2))
let len = Base.Hash_set.length b
let _ = print_endline ("length is " ^ string_of_int len)
