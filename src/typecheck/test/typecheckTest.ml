open OUnit
open Core
open GenCode
open Dt
open ExecuteCode
open FlattenInstrForPrint
open ParseTree
open TypeChecker



let test1 () =
  let prog = program_of_sexp string_of_sexp
       (Sexp.of_string "(Program(Sentence((D(TypeDef cute()((TypeDefRight dusk()))))(D(TypeDef list(A)((TypeDefRight nil())(TypeDefRight cons((TypeVar A)(TypeCons list((TypeVar A))))))))(P(PredDef addOne((TypeCons list((TypeCons cute())))(TypeCons list((TypeCons cute()))))))(C(Clause(Atom addOne((TVar X)(TFun cons((TFun dusk())(TVar X)))))()))))(Resolvant((CAT(Atom addOne((TFun nil())(TVar X)))))))")

  in let res = typeCheck true prog
  in assert_equal res ()



let suite = "Code gen tests" >::: ["test1" >:: test1]

let _ =
  run_test_tt_main suite
