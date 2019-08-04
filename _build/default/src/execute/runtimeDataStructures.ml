open Core
open Dt
open ParseTree
open StackImplementation

type result = variable array [@@deriving sexp]

type trailVal = Var of heapValue ref | Label [@@deriving show]

type intnum = int [@@deriving show]

type heap = heapValue array [@@deriving show]

type varr = variable array [@@deriving show]

type trail = trailVal node option [@@deriving show]

type choicePoint =
  { mutable nextOptionPointer: int
  ; stack: stackVal
  ; arguments: variable array
  ; returnAddr: int
  ; returnCps: choicePoint node option
  ; returnTrailPoint: trailVal node option }
[@@deriving show]

and choicePointStack = choicePoint node option [@@deriving show]

and environment =
  { vars: variable array
  ; mutable returnAddress: int
  ; callerCps: choicePointStack
  ; callerTrailpoint: trailVal node option }
[@@deriving show]

and stackVal = environment node option [@@deriving show]

type compState =
  { mutable currentStr: heapValue sexp_array * intnum
  ; mutable cp: int
  ; mutable returnAddress: int
  ; mutable returnCps: choicePoint node option
  ; mutable returnTrailPoint: trailVal node option
  ; mutable arguments: variable sexp_array
  ; mutable temps: variable sexp_array
  ; mutable choicePoints: choicePointStack
  ; mutable envStack: stackVal
  ; mutable trail: trailVal node option
  ; mutable infinal: bool }

module type DriverTyp = sig
  val execute : compState -> environment option
end

module type InstructionFunctionsTyp = sig
  val performArith :
       compState
    -> location
    -> intnum
    -> environment node
    -> (intnum -> intnum -> intnum)
    -> environment option

  val evalIs :
       variable
    -> intnum
    -> variable sexp_array
    -> intnum
    -> compState
    -> environment option

  val getinta : variable -> int -> compState -> environment option

  val getStructure :
    int -> int -> variable array -> int -> compState -> environment option

  val putStructure :
       intnum
    -> intnum
    -> variable sexp_array
    -> intnum
    -> compState
    -> environment option

  val unifyInFromHeap :
    variable sexp_array -> intnum -> compState -> environment option

  val setValue :
    variable sexp_array -> intnum -> compState -> environment option
end
