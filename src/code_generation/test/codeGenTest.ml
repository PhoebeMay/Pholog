open OUnit
open Core
open GenCode
open Dt
(*
let testMachineNoTc parsetree code =
  assert_equal (execute input_str false) expected_result *)



let test1 () =
  (*
  nextTo(house,car).
  nextTo(tree,tree).
  ?- nextTo(X,X)
  *)

  let functT = Hashtbl.of_alist_exn functionTKeyImp (functTlist_of_sexp (Sexp.of_string "(((AbstractF(nextTo
2))(((Clause(Atom nextTo((TFun tree())(TFun tree())))())0)((Clause(Atom nextTo((TFun house())(TFun
car())))())0))))
"))

  in let query = query_of_sexp (Sexp.of_string "(((CAT(Atom nextTo((TVar(E(Env 0)))(TVar(E(Env 0)))))))1)")

  in let expected_result =  Sexp.of_string "((((AbstractF(query 0))((0((Allocate 1)(PutVariable(E(Env
0))(Arg 0))(PutValue(E(Env 0))(Arg 1))(Call(AbstractF(nextTo 2)))Finish))))((AbstractF(nextTo
2))((0((TryMeElse(AbstractC(AbstractF(nextTo 2))1))(Allocate 0)(GetStructureA(0 0)(Arg 0))(GetStructureA(1
0)(Arg 1))Deallocate))(1(TrustMe(Allocate 0)(GetStructureA(2 0)(Arg 0))(GetStructureA(2 0)(Arg
1))Deallocate)))))((maxArg 2)(maxTemp 1))((0 house)(1 car)(2 tree)))
"

  in let result = genCode functT query
  in let result_sexp = sexp_of_code result
  in assert_equal result_sexp expected_result



let suite = "Code gen tests" >::: ["test1" >:: test1]

let _ =
  run_test_tt_main suite
