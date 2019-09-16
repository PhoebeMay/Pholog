open Core
open Dt

type res = variableFlat list [@@deriving show]

let rec flattenHv structMap x=
  match x with
  | B(x) -> (flattenHv structMap !x)
  | InitH -> InitHF
  | UnboundVar -> UnboundVarF
  | StrPointer(s,arr) ->
    StrPointerF(Hashtbl.find_exn structMap s,
                Array.map ~f:(flattenHv structMap) arr )
  | Int(n) -> IntF(n)

let flattenSv structMap v = match v with
  | InitV -> InitVF
  | HeapPointer(x) -> HeapPointerF(flattenHv structMap x)
  | TempInt(n) -> TempIntF(n)
