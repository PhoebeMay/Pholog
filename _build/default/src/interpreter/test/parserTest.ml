open OUnit
open ParseTree

let testLexerParser input_str expected_result =
  let lexbuf = Lexing.from_string input_str in
  let result = Parser.main Lexer.token lexbuf in
  assert_equal result expected_result

let basic_test () =
  testLexerParser "animal(C):-cat(C)."
    (Program
       ( Sentence
           [Clause (Atom ("animal", [TVar "C"]), [Atom ("cat", [TVar "C"])])]
       , Resolvant [] ))

let no_body_test () =
  testLexerParser "?-cat(C)"
    (Program (Sentence [], Resolvant [Atom ("cat", [TVar "C"])]))

let functor_test () =
  testLexerParser "inStorePair(X,Y):-inStore(pair(X,Y))."
    (Program
       ( Sentence
           [ Clause
               ( Atom ("inStorePair", [TVar "X"; TVar "Y"])
               , [Atom ("inStore", [TFun ("pair", [TVar "X"; TVar "Y"])])] ) ]
       , Resolvant [] ))

let multi_clause_test () =
  testLexerParser "fluffy(X) :- cat(X).\n     animal(X) :- cat(x)."
    (Program
       ( Sentence
           [ Clause (Atom ("fluffy", [TVar "X"]), [Atom ("cat", [TVar "X"])])
           ; Clause
               (Atom ("animal", [TVar "X"]), [Atom ("cat", [TFun ("x", [])])])
           ]
       , Resolvant [] ))

let multi_resolvant_test () =
  testLexerParser
    "fluffy(X) :- cat(X), animal(build(   X)) .\n\n\
    \    ?- doggie(X),bigAnimal(build(X))"
    (Program
       ( Sentence
           [ Clause
               ( Atom ("fluffy", [TVar "X"])
               , [ Atom ("cat", [TVar "X"])
                 ; Atom ("animal", [TFun ("build", [TVar "X"])]) ] ) ]
       , Resolvant
           [ Atom ("doggie", [TVar "X"])
           ; Atom ("bigAnimal", [TFun ("build", [TVar "X"])]) ] ))

let zero_term_in_pred_test () =
  testLexerParser "cat."
    (Program (Sentence [Clause (Atom ("cat", []), [])], Resolvant []))

let many_term_in_pred_test () =
  testLexerParser "cat(A, B, Variablename, VVVVV)."
    (Program
       ( Sentence
           [ Clause
               ( Atom
                   ( "cat"
                   , [TVar "A"; TVar "B"; TVar "Variablename"; TVar "VVVVV"] )
               , [] ) ]
       , Resolvant [] ))

let atom_only_functor () =
  testLexerParser
    "\n  animal(X) :- cat(X).\n  cat(fluffy).\n\n  ?- animal(fluffy)\n  "
    (Program
       ( Sentence
           [ Clause (Atom ("animal", [TVar "X"]), [Atom ("cat", [TVar "X"])])
           ; Clause (Atom ("cat", [TFun ("fluffy", [])]), []) ]
       , Resolvant [Atom ("animal", [TFun ("fluffy", [])])] ))

let suite =
  "OUnit Lexer + Parser Tests"
  >::: [ "basic_test" >:: basic_test
       ; "no_body_test" >:: no_body_test
       ; "functor_test" >:: functor_test
       ; "multi_clause_test" >:: multi_clause_test
       ; "multi_resolvant_test" >:: multi_resolvant_test
       ; "zero_term_in_pred_test" >:: zero_term_in_pred_test
       ; "many_term_in_pred_test" >:: many_term_in_pred_test ]

let _ = run_test_tt_main suite
