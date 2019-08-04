open OUnit
open Core
open GenCode
open Dt
open ExecuteCode
open FlattenInstrForPrint

(*
let testMachineNoTc parsetree code =
  assert_equal (execute input_str false) expected_result *)

let test1 () =
  (*
  nextTo(house,car).
  nextTo(tree,tree).
  ?- nextTo(X,X)
  *)
  let instr =
    writtenInstr_of_sexp
      (Sexp.of_string
         "((nums((maxArg 2)(maxTemp 1)))(code((Allocate\n\
          1)(PutVariable(E(Env 0))(Arg 0))(PutValue(E(Env 0))(Arg \
          1))(Call(PositionF 5))Finish(TryMeElse(PositionC\n\
          10))(Allocate 0)(GetStructureA(0 0)(Arg 0))(GetStructureA(1 0)(Arg \
          1))Deallocate TrustMe(Allocate\n\
          0)(GetStructureA(2 0)(Arg 0))(GetStructureA(2 0)(Arg \
          1))Deallocate))(structMap((0 house)(1 car)(2 tree))))\n")
  in
  let res = executeInstructions instr in
  let res_sexp = sexp_of_result res in
  let expected_result = Sexp.of_string "((HeapPointer(B(StrPointer 2()))))" in
  assert_equal res_sexp expected_result

let suite = "Code gen tests" >::: ["test1" >:: test1]

let _ = run_test_tt_main suite
