open Core
open ParseTree

exception Oops

exception Fail

exception Err of string

type arg = Arg of int [@@deriving show, sexp]

type env = Env of int [@@deriving sexp, show]

type temp = Temp of int [@@deriving show, sexp]

type intnum = int [@@deriving show, sexp]

type clauseAndNum = env clause * int [@@deriving show, sexp]

type location = (* A of arg  *)
  | E of env | T of temp [@@deriving show, sexp]

type structure = int * int [@@deriving show, sexp]

(* type structureLookup = (int,string) Base.Hashtbl.t *)

let intImp =
  ( module struct
    type t = int

    let compare = Pervasives.compare

    let sexp_of_t = sexp_of_int

    let hash x = x
  end : Base__.Hash_set_intf.Key
    with type t = int )

type structureLookup = string Base.Hashtbl.M(Int).t [@@deriving sexp]

type varLookup = var Base.Hashtbl.M(Int).t [@@deriving sexp]

type structMapGen = {
  mapping : (string, int) Base.Hashtbl.t;
  mutable nextFree : int;
}

type functionId = AbstractF of (string * int) | PositionF of int
[@@deriving sexp, show]

(* type row = functionId * clauseAndNum  [@@deriving show] *)
type clausePos = AbstractC of functionId * int | PositionC of int
[@@deriving sexp, show]

let envImp =
  ( module struct
    type t = env

    let compare = Pervasives.compare

    let sexp_of_t = sexp_of_env

    let hash (Env x) = x
  end : Base__.Hash_set_intf.Key
    with type t = env )

let locationImp =
  ( module struct
    type t = location

    let compare = Pervasives.compare

    let sexp_of_t = sexp_of_location

    let hash x = match x with E (Env n) -> n * 2 | T (Temp n) -> (n * 2) + 1
  end : Base__.Hash_set_intf.Key
    with type t = location )

let functionTKeyImp =
  ( module struct
    type t = functionId

    let compare = Pervasives.compare

    let sexp_of_t = sexp_of_functionId

    let hash x =
      match x with
      | PositionF n -> n
      | AbstractF (name, arity) ->
          let hash = Hash.create ~seed:(String.hash name) () in
          let hash2 = Hash.fold_int hash arity in
          Hash.get_hash_value hash2
  end : Base__.Hashtbl_intf.Key
    with type t = functionId )

let strKeyImp =
  ( module struct
    type t = string

    let compare = Pervasives.compare

    let sexp_of_t = sexp_of_string

    let hash x = String.hash x
  end : Base__.Hashtbl_intf.Key
    with type t = string )

type strInt = string * int [@@deriving show, sexp]

let strIntKeyImp =
  ( module struct
    type t = string * int

    let compare = Pervasives.compare

    let sexp_of_t = sexp_of_strInt

    let hash (x, y) =
      let hash = Hash.create ~seed:(String.hash x) () in
      let hash2 = Hash.fold_int hash y in
      Hash.get_hash_value hash2
  end : Base__.Hashtbl_intf.Key
    with type t = string * int )

(*
let fidKeyImp =
  (module struct
    type t = functionId
    let compare = Pervasives.compare
    let sexp_of_t = sexp_of_functionId
    let hash (name,argnum) =
      let hash = Hash.create ~seed:(String.hash name) ()
      in let hash2 = Hash.fold_int hash argnum
      in Hash.get_hash_value hash2
  end :  Base__.Hashtbl_intf.Key with type t = functionId) *)

type instruction =
  | TryMeElse of clausePos
  | RetryMeElse of clausePos
  | TrustMe
  | GetVariable of location * arg
  (*Variable not yet seen*)
  | GetValue of location * arg
  | GetStructureA of structure * arg
  | GetStructureT of structure * temp
  | GetInt of arg * int
  | StructGetVariable of location
  | StructGetValue of location
  | StructGetInt of int
  | PutStructureA of structure * arg
  | PutStructureT of structure * temp
  | PutIntA of arg * int
  | PutIntT of temp * int
  | PutVariable of location * arg
  | PutValue of location * arg
  | SetVariable of location
  | SetValue of location
  | SetInt of int
  | Allocate of int
  | Deallocate
  | DeallocateBeforeLastCall
  | InitAcc of temp * location
  | Add of temp * location
  | AddI of temp * int
  | Sub of temp * location
  | SubI of temp * int
  | Is of location * temp
  | Call of functionId
  | CallAfterDealloc of functionId
  | ClearTemp of temp
  | Finish
  | Empty
  (*Used to populate initially *)
  | RmCps
  | Backtrack
[@@deriving show, sexp]

type inner = clauseAndNum list [@@deriving show, sexp]

type arrLens = { mutable maxArg : int; mutable maxTemp : int }
[@@deriving sexp]

type writtenInstr = {
  nums : arrLens;
  code : instruction sexp_array;
  structMap : structureLookup;
}
[@@deriving sexp]

type heapValue =
  | B of heapValue ref
  | InitH
  | UnboundVar
  | StrPointer of int * heapValue array
  | Int of int
[@@deriving show, sexp]

(* and heapVar = {mutable v : heapValue} [@@deriving show] *)

type variable = InitV | HeapPointer of heapValue | TempInt of int
[@@deriving show, sexp]

type heapValueFlat =
  | UnboundVarF
  | InitHF
  | StrPointerF of string * heapValueFlat array
  | IntF of int
[@@deriving show]

type variableFlat = InitVF | HeapPointerF of heapValueFlat | TempIntF of int
[@@deriving show]

type ans = variableFlat list [@@deriving show]

type infv = (var * int) typeins [@@deriving show]

type typei = var * int [@@deriving show]
