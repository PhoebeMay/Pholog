open Interp
open OUnit
open ParseTree

exception UnifiyFail

exception False

let testInterpreter input_str expected_result =
  let lexbuf = Lexing.from_string input_str in
  let parseTree = Parser.main Lexer.token lexbuf in
  let result = interpret parseTree in
  assert_equal result expected_result

let basic_true () =
  testInterpreter
    "\n  animal(X) :- cat(X).\n  cat(fluffy).\n\n  ?- animal(fluffy)\n  "
    (Ans (Resolvant [Atom ("animal", [TFun ("fluffy", [])])]))

let basic_false () =
  testInterpreter
    "\n  animal(X) :- cat(X).\n  cat(fluffy).\n\n  ?- animal(prickly)\n  " Fail

let unification () =
  testInterpreter "\n  animal(dusk).\n  ?- animal(X)\n  "
    (Ans (Resolvant [Atom ("animal", [TFun ("dusk", [])])]))

let basic_backtrack () =
  testInterpreter
    "\n\
    \  animal(craig).\n\
    \  animal(dusk).\n\
    \  dog(dusk).\n\
    \  ?- animal(X), dog(X)\n\
    \  "
    (Ans
       (Resolvant
          [ Atom ("animal", [TFun ("dusk", [])])
          ; Atom ("dog", [TFun ("dusk", [])]) ]))

let repeat_var () =
  testInterpreter
    "\n  nextTo(house,car).\n  nextTo(tree,tree).\n  ?- nextTo(X,X)\n  "
    (Ans (Resolvant [Atom ("nextTo", [TFun ("tree", []); TFun ("tree", [])])]))

let two_var () =
  testInterpreter
    "\n    person(craig).\n    dog(dusk).\n    ?- person(X), dog(Y)\n    "
    (Ans
       (Resolvant
          [ Atom ("person", [TFun ("craig", [])])
          ; Atom ("dog", [TFun ("dusk", [])]) ]))

let false_1 () =
  testInterpreter
    "\n    person(craig).\n    dog(dusk).\n    ?- person(X), dog(X)\n    " Fail

let priority_order () =
  testInterpreter
    "\n    animal(dusk).\n    animal(craig).\n    ?- animal(X)\n    "
    (Ans (Resolvant [Atom ("animal", [TFun ("dusk", [])])]))

let t9 () =
  testInterpreter
    "\n   person(craig).\n   dog(dusk).\n   ?- person(X), dog(Y)\n   "
    (Ans
       (Resolvant
          [ Atom ("person", [TFun ("craig", [])])
          ; Atom ("dog", [TFun ("dusk", [])]) ]))

let no_swaps () =
  testInterpreter
    "\ng(dusk).\nh(craig).\n\nf(A,B) :- g(A), h(B).\n\n?-f(Y,X)\n"
    (Ans (Resolvant [Atom ("f", [TFun ("dusk", []); TFun ("craig", [])])]))

let swaps1 () =
  testInterpreter
    "\ng(dusk).\nh(craig).\n\nf(X,Y) :- g(X), h(Y).\n\n?-f(Y,X)\n  "
    (Ans (Resolvant [Atom ("f", [TFun ("dusk", []); TFun ("craig", [])])]))

let swaps2 () =
  testInterpreter
    "\ng(dusk).\nh(craig).\n\nf(X,Y) :- g(Y), h(X).\n\n?-f(Y,X)\n"
    (Ans (Resolvant [Atom ("f", [TFun ("craig", []); TFun ("dusk", [])])]))

let structarg () =
  testInterpreter "\nplant(flower(X)).\n\n?- plant(X)\n"
    (Ans
       (Resolvant
          [Atom ("plant", [TFun ("flower", [TVar "(Interp.Argument 0)"])])]))

let addOne () =
  testInterpreter
    "\n\
     add(X,zero,X).\n\
     add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\n\n\n\
     ?- add(zero,s(zero),X)\n\n"
    (Ans
       (Resolvant
          [ Atom
              ( "add"
              , [ TFun ("zero", [])
                ; TFun ("s", [TFun ("zero", [])])
                ; TFun ("s", [TFun ("zero", [])]) ] ) ]))

let onePlusOne () =
  testInterpreter
    "\n\
     add(X,zero,X).\n\
     add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\n\n\
     ?- add(s(zero),s(zero),X)\n\n"
    (Ans
       (Resolvant
          [ Atom
              ( "add"
              , [ TFun ("s", [TFun ("zero", [])])
                ; TFun ("s", [TFun ("zero", [])])
                ; TFun ("s", [TFun ("s", [TFun ("zero", [])])]) ] ) ]))

let alpha1 () =
  testInterpreter "\nf(X).\n\n?- f(struct(X))\n"
    (Ans (Resolvant [Atom ("f", [TFun ("struct", [TVar "(Interp.Inner 0)"])])]))

let alpha2 () =
  testInterpreter "\nf(struct(X)).\n\n?- f(X)\n"
    (Ans
       (Resolvant [Atom ("f", [TFun ("struct", [TVar "(Interp.Argument 0)"])])]))

let alpha3 () =
  testInterpreter "\ng(build(X)).\nf(struct(X)) :- g(X).\n\n?- f(X)\n"
    (Ans
       (Resolvant
          [ Atom
              ( "f"
              , [ TFun
                    ("struct", [TFun ("build", [TVar "(Interp.Argument 0)"])])
                ] ) ]))

let zeroTwo () =
  testInterpreter
    "\n\
     add(X,zero,X).\n\
     add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\n\n\
     ?- add(zero,s(s(zero)),X)\n\n"
    (Ans
       (Resolvant
          [ Atom
              ( "add"
              , [ TFun ("zero", [])
                ; TFun ("s", [TFun ("s", [TFun ("zero", [])])])
                ; TFun ("s", [TFun ("s", [TFun ("zero", [])])]) ] ) ]))

let fib2 () =
  testInterpreter
    "\n\n\n\
     add(X,zero,X).\n\
     add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\n\n\
     fib(zero,zero).\n\
     fib(s(zero),s(zero)).\n\n\
     fib(s(s(X)),Y) :- fib(X,A), fib(s(X),B), add(A,B,Y).\n\n\
     ?- fib(s(s(zero)),X)\n\n"
    (Ans
       (Resolvant
          [ Atom
              ( "fib"
              , [ TFun ("s", [TFun ("s", [TFun ("zero", [])])])
                ; TFun ("s", [TFun ("zero", [])]) ] ) ]))

let twoThree () =
  testInterpreter
    "\n\
     add(X,zero,X).\n\
     add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\n\n\
     ?- add(s(s(zero)),s(s(s(zero))),s(X))\n\n"
    (Ans
       (Resolvant
          [ Atom
              ( "add"
              , [ TFun ("s", [TFun ("s", [TFun ("zero", [])])])
                ; TFun ("s", [TFun ("s", [TFun ("s", [TFun ("zero", [])])])])
                ; TFun
                    ( "s"
                    , [ TFun
                          ( "s"
                          , [ TFun
                                ( "s"
                                , [ TFun
                                      ("s", [TFun ("s", [TFun ("zero", [])])])
                                  ] ) ] ) ] ) ] ) ]))

let addOneP () =
  testInterpreter
    "\n\
     add(X,zero,X).\n\
     add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\n\
     addOne(X,Y) :- add(X,s(zero),A), add(A,zero,Y).\n\n\
     ?- addOne(zero,A)\n"
    (Ans
       (Resolvant
          [ Atom
              ("addOne", [TFun ("zero", []); TFun ("s", [TFun ("zero", [])])])
          ]))

let fib4 () =
  testInterpreter
    "\n\
     add(X,zero,X).\n\
     add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\n\n\
     fib(zero,zero).\n\
     fib(s(zero),s(zero)).\n\n\
     fib(s(s(X)),Y) :- fib(X,A), fib(s(X),B), add(A,B,Y).\n\n\
     ?- fib(s(s(s(s(zero)))),X)\n"
    (Ans
       (Resolvant
          [ Atom
              ( "fib"
              , [ TFun
                    ( "s"
                    , [ TFun
                          ("s", [TFun ("s", [TFun ("s", [TFun ("zero", [])])])])
                      ] )
                ; TFun ("s", [TFun ("s", [TFun ("s", [TFun ("zero", [])])])])
                ] ) ]))

let fib13 () =
  testInterpreter
    "\n\
     add(X,zero,X).\n\
     add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\n\n\
     fib(zero,zero).\n\
     fib(s(zero),s(zero)).\n\n\
     fib(s(s(X)),Y) :- fib(X,A), fib(s(X),B), add(A,B,Y).\n\n\
     ?- fib(s(s(s(s(s(s(s(zero))))))),X)\n"
    (Ans
       (Resolvant
          [ Atom
              ( "fib"
              , [ TFun
                    ( "s"
                    , [ TFun
                          ( "s"
                          , [ TFun
                                ( "s"
                                , [ TFun
                                      ( "s"
                                      , [ TFun
                                            ( "s"
                                            , [ TFun
                                                  ( "s"
                                                  , [ TFun
                                                        ( "s"
                                                        , [TFun ("zero", [])]
                                                        ) ] ) ] ) ] ) ] ) ] )
                      ] )
                ; TFun
                    ( "s"
                    , [ TFun
                          ( "s"
                          , [ TFun
                                ( "s"
                                , [ TFun
                                      ( "s"
                                      , [ TFun
                                            ( "s"
                                            , [ TFun
                                                  ( "s"
                                                  , [ TFun
                                                        ( "s"
                                                        , [ TFun
                                                              ( "s"
                                                              , [ TFun
                                                                    ( "s"
                                                                    , [ TFun
                                                                          ( "s"
                                                                          , [ TFun
                                                                               ( 
                                                                               "s"
                                                                               , 
                                                                               [ 
                                                                               TFun
                                                                               ( 
                                                                               "s"
                                                                               , 
                                                                               [ 
                                                                               TFun
                                                                               ( 
                                                                               "s"
                                                                               , 
                                                                               [ 
                                                                               TFun
                                                                               ( 
                                                                               "zero"
                                                                               , 
                                                                               []
                                                                               )
                                                                               ]
                                                                               )
                                                                               ]
                                                                               )
                                                                               ]
                                                                               )
                                                                            ]
                                                                          ) ]
                                                                    ) ] ) ] )
                                                    ] ) ] ) ] ) ] ) ] ) ] ) ]
              ) ]))

let pairOne () =
  testInterpreter
    "\n\
     ll(one,two).\n\n\
     unif(X,X).\n\n\
     less(null,Z, null).\n\
     less(pair(H,T), Bound, Result) :- ll(H,Bound), less(T, Bound, NewRes), \
     unif(Result, pair(H, NewRes)).\n\n\
     ?- less(pair(one,null),two,Result)\n"
    (Ans
       (Resolvant
          [ Atom
              ( "less"
              , [ TFun ("pair", [TFun ("one", []); TFun ("null", [])])
                ; TFun ("two", [])
                ; TFun ("pair", [TFun ("one", []); TFun ("null", [])]) ] ) ]))

let quick21 () =
  testInterpreter
    "\n\
    \  ll(one,three).\n\
    \  ll(one,two).\n\n\
    \  ll(two,three).\n\n\
    \  geq(one,one).\n\n\
    \  geq(two,one).\n\
    \  geq(two,two).\n\n\
    \  geq(three,one).\n\
    \  geq(three,two).\n\
    \  geq(three,three).\n\n\
    \  unif(X,X).\n\n\
    \  less(null,Z, null).\n\
    \  less(pair(H,T), Bound, Result) :- ll(H,Bound), less(T, Bound, NewRes), \
     unif(Result, pair(H, NewRes)).\n\
    \  less(pair(H,T), Bound, Result) :- geq(H,Bound), less(T, Bound, Result).\n\n\
    \  geq(null,Z, null).\n\
    \  geq(pair(H,T), Bound, Result) :- geq(H,Bound), geq(T, Bound, NewRes), \
     unif(Result, pair(H, NewRes)).\n\
    \  geq(pair(H,T), Bound, Result) :- ll(H, Bound), geq(T, Bound, Result).\n\n\
    \  part(pair(H,T), Low, Mid, Up) :- unif(Mid, H), less(T,H, Low), \
     geq(T,H, Up).\n\n\
    \  join(null,Mid, Up, Result) :- unif(Result, pair(Mid,Up)).\n\
    \  join(pair(H,T),Mid, Up, Result) :- join(T, Mid, Up, NewRes), \
     unif(Result, pair(H,NewRes)).\n\n\
    \  quick(null, null).\n\
    \  quick(pair(X,null), X).\n\
    \  quick(pair(X, pair(Y, null)), Result) :- geq(Y, X), unif(Result, \
     pair(X, pair(Y, null))).\n\n\
    \  quick(X, Result) :-\n\
    \    part(X, Low, Mid, Up),\n\
    \    quick(Low, ResLow),\n\
    \    quick(Up, ResUp),\n\
    \    join(ResLow, Mid, ResUp, Result).\n\n\n\
    \  ?- quick(pair(two,pair(one,null)),X)\n\n\
    \  "
    (Ans
       (Resolvant
          [ Atom
              ( "quick"
              , [ TFun
                    ( "pair"
                    , [ TFun ("two", [])
                      ; TFun ("pair", [TFun ("one", []); TFun ("null", [])]) ]
                    )
                ; TFun
                    ( "pair"
                    , [ TFun ("one", [])
                      ; TFun ("pair", [TFun ("two", []); TFun ("null", [])]) ]
                    ) ] ) ]))

let quick213 () =
  testInterpreter
    "\n\
     ll(one,three).\n\
     ll(one,two).\n\n\
     ll(two,three).\n\n\
     geq(one,one).\n\n\
     geq(two,one).\n\
     geq(two,two).\n\n\
     geq(three,one).\n\
     geq(three,two).\n\
     geq(three,three).\n\n\
     unif(X,X).\n\n\
     less(null,Z, null).\n\
     less(pair(H,T), Bound, Result) :- ll(H,Bound), less(T, Bound, NewRes), \
     unif(Result, pair(H, NewRes)).\n\
     less(pair(H,T), Bound, Result) :- geq(H,Bound), less(T, Bound, Result).\n\n\
     geq(null,Z, null).\n\
     geq(pair(H,T), Bound, Result) :- geq(H,Bound), geq(T, Bound, NewRes), \
     unif(Result, pair(H, NewRes)).\n\
     geq(pair(H,T), Bound, Result) :- ll(H, Bound), geq(T, Bound, Result).\n\n\
     part(pair(H,T), Low, Mid, Up) :- unif(Mid, H), less(T,H, Low), geq(T,H, \
     Up).\n\n\
     join(null,Mid, Up, Result) :- unif(Result, pair(Mid,Up)).\n\
     join(pair(H,T),Mid, Up, Result) :- join(T, Mid, Up, NewRes), \
     unif(Result, pair(H,NewRes)).\n\n\
     quick(null, null).\n\
     quick(pair(X,null), X).\n\
     quick(pair(X, pair(Y, null)), Result) :- geq(Y, X), unif(Result, pair(X, \
     pair(Y, null))).\n\n\
     quick(X, Result) :-\n\
    \  part(X, Low, Mid, Up),\n\
    \  quick(Low, ResLow),\n\
    \  quick(Up, ResUp),\n\
    \  join(ResLow, Mid, ResUp, Result).\n\n\n\
     ?- quick(pair(two,pair(one,pair(three,null))),X)\n\n"
    (Ans
       (Resolvant
          [ Atom
              ( "quick"
              , [ TFun
                    ( "pair"
                    , [ TFun ("two", [])
                      ; TFun
                          ( "pair"
                          , [ TFun ("one", [])
                            ; TFun
                                ( "pair"
                                , [TFun ("three", []); TFun ("null", [])] ) ]
                          ) ] )
                ; TFun
                    ( "pair"
                    , [ TFun ("one", [])
                      ; TFun ("pair", [TFun ("two", []); TFun ("three", [])])
                      ] ) ] ) ]))

let sort21312 () =
  testInterpreter
    "\n\
    \  ll(one,three).\n\
    \  ll(one,two).\n\n\
    \  ll(two,three).\n\n\
    \  geq(one,one).\n\n\
    \  geq(two,one).\n\
    \  geq(two,two).\n\n\
    \  geq(three,one).\n\
    \  geq(three,two).\n\
    \  geq(three,three).\n\n\
    \  unif(X,X).\n\n\
    \  less(null,Z, null).\n\
    \  less(pair(H,T), Bound, Result) :- ll(H,Bound), less(T, Bound, NewRes), \
     unif(Result, pair(H, NewRes)).\n\
    \  less(pair(H,T), Bound, Result) :- geq(H,Bound), less(T, Bound, Result).\n\n\
    \  geq(null,Z, null).\n\
    \  geq(pair(H,T), Bound, Result) :- geq(H,Bound), geq(T, Bound, NewRes), \
     unif(Result, pair(H, NewRes)).\n\
    \  geq(pair(H,T), Bound, Result) :- ll(H, Bound), geq(T, Bound, Result).\n\n\
    \  part(pair(H,T), Low, Mid, Up) :- unif(Mid, H), less(T,H, Low), \
     geq(T,H, Up).\n\n\
    \  join(null,Mid, Up, Result) :- unif(Result, pair(Mid,Up)).\n\
    \  join(pair(H,T),Mid, Up, Result) :- join(T, Mid, Up, NewRes), \
     unif(Result, pair(H,NewRes)).\n\n\
    \  quick(null, null).\n\
    \  quick(pair(X,null), X).\n\
    \  quick(pair(X, pair(Y, null)), Result) :- geq(Y, X), unif(Result, \
     pair(X, pair(Y, null))).\n\n\
    \  quick(X, Result) :-\n\
    \    part(X, Low, Mid, Up),\n\
    \    quick(Low, ResLow),\n\
    \    quick(Up, ResUp),\n\
    \    join(ResLow, Mid, ResUp, Result).\n\n\n\
    \  ?- quick(pair(two,pair(one,pair(three,pair(one,pair(two,null))))),X)\n\n\
    \  "
    (Ans
       (Resolvant
          [ Atom
              ( "quick"
              , [ TFun
                    ( "pair"
                    , [ TFun ("two", [])
                      ; TFun
                          ( "pair"
                          , [ TFun ("one", [])
                            ; TFun
                                ( "pair"
                                , [ TFun ("three", [])
                                  ; TFun
                                      ( "pair"
                                      , [ TFun ("one", [])
                                        ; TFun
                                            ( "pair"
                                            , [ TFun ("two", [])
                                              ; TFun ("null", []) ] ) ] ) ] )
                            ] ) ] )
                ; TFun
                    ( "pair"
                    , [ TFun ("one", [])
                      ; TFun
                          ( "pair"
                          , [ TFun ("one", [])
                            ; TFun
                                ( "pair"
                                , [ TFun ("two", [])
                                  ; TFun
                                      ( "pair"
                                      , [ TFun ("two", [])
                                        ; TFun
                                            ( "pair"
                                            , [ TFun ("three", [])
                                              ; TFun ("null", []) ] ) ] ) ] )
                            ] ) ] ) ] ) ]))

let suite =
  "OUnit Lexer + Parser Tests"
  >::: [ "basic_true" >:: basic_true
       ; "basic_false" >:: basic_false
       ; "unification" >:: unification
       ; "basic_backtrack" >:: basic_backtrack
       ; "repeat_var" >:: repeat_var
       ; "two_var" >:: two_var
       ; "false_1" >:: false_1
       ; "priority_order" >:: priority_order
       ; "t9" >:: t9
       ; "no_swaps" >:: no_swaps
       ; "swaps1" >:: swaps1
       ; "swaps2" >:: swaps2
       ; "structarg" >:: structarg
       ; "addOne" >:: addOne
       ; "onePlusOne" >:: onePlusOne
       ; "alpha1" >:: alpha1
       ; "alpha2" >:: alpha2
       ; "alpha3" >:: alpha3
       ; "zeroTwo" >:: zeroTwo
       ; "addOneP" >:: addOneP
       ; "fib4" >:: fib4
       ; "fib13" >:: fib13
       ; "pairOne" >:: pairOne
       ; "quick21" >:: quick21
       ; "quick213" >:: quick213
       ; "sort21312" >:: sort21312 ]

let _ = run_test_tt_main suite
