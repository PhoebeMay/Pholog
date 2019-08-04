open Core
open Dt
open RuntimeDataStructures

(* let createChoicePoint n stack args ra = {nextOptionPointer = n; stack = stack; arguments = args; returnAddr = ra} *)

let updateChoicePoint n c = c.nextOptionPointer <- n

type arguments = variable array ref [@@deriving show]

let updateEnvRA c n = c.returnAddress <- n

exception InstrNotMatch of string

let createEnvironment n ra previousStackHead cps tv =
  (* logError (fun m -> m "Create env of size %a" pp_intnum n); *)
  let newArr = Array.create ~len:n InitV in
  let newEnv =
    {vars= newArr; returnAddress= ra; callerCps= cps; callerTrailpoint= tv}
  in
  StackImplementation.createNew newEnv ~prev:previousStackHead

exception Finished of environment
