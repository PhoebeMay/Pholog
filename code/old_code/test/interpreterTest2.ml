open Interp
open OUnit
open ParseTree

exception UnifiyFail
exception False

let testInterpreter input_str expected_result =
  let lexbuf = Lexing.from_string input_str in
  let parseTree = Parser.main Lexer.token lexbuf in
  let result = (interpret parseTree) in
  assert_equal result expected_result

let basic_true () =
  testInterpreter
    "
  animal(X) :- cat(X).
  cat(fluffy).

  ?- animal(fluffy)
  "
    (Ans(Resolvant([
         Atom("animal",[TFun("fluffy",[])])
       ])))

let basic_false () =
  testInterpreter
    "
  animal(X) :- cat(X).
  cat(fluffy).

  ?- animal(prickly)
  "
    (Fail)

let unification () =
  testInterpreter "
  animal(dusk).
  ?- animal(X)
  "
    (Ans(Resolvant([
         Atom("animal",[TFun("dusk",[])])
       ])))


let basic_backtrack () =
  testInterpreter
    "
  animal(craig).
  animal(dusk).
  dog(dusk).
  ?- animal(X), dog(X)
  "
    (Ans(Resolvant([
         Atom("animal",[TFun("dusk",[])]);
         Atom("dog",[TFun("dusk",[])])
       ])))

let repeat_var () =
  testInterpreter
    "
  nextTo(house,car).
  nextTo(tree,tree).
  ?- nextTo(X,X)
  "
    (Ans(Resolvant([Atom("nextTo",[TFun("tree",[]);TFun("tree",[])])])))


let two_var () =
  testInterpreter
    "
    person(craig).
    dog(dusk).
    ?- person(X), dog(Y)
    "
    (Ans(Resolvant([Atom("person",[TFun("craig",[])]);Atom("dog",[TFun("dusk",[])])])))

let false_1 () =
  testInterpreter
    "
    person(craig).
    dog(dusk).
    ?- person(X), dog(X)
    "
    (Fail)

let priority_order () =
  testInterpreter
    "
    animal(dusk).
    animal(craig).
    ?- animal(X)
    "
    (Ans(Resolvant([Atom("animal",[TFun("dusk",[])])])))

let suite = "OUnit Lexer + Parser Tests" >::: [
    "basic_true" >:: basic_true;
    "basic_false" >:: basic_false;
    "unification" >:: unification;
    "basic_backtrack" >:: basic_backtrack;
    "repeat_var" >:: repeat_var;
    "two_var" >:: two_var;
    "false_1" >:: false_1;
    "priority_order" >:: priority_order;
  ]

let _ =
  run_test_tt_main suite
