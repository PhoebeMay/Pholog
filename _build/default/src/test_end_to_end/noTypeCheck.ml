open OUnit
open Dt
open Runall
open Logging

let testMachineNoTc input_str expected_result =
  let res = execute input_str false in
  match res with
  | Some r -> assert_equal r expected_result
  | None -> raise Oops

let test1 () =
  testMachineNoTc
    "\n\
    \    nextTo(house,car).\n\
    \    nextTo(tree,tree).\n\
    \    ?- nextTo(X,X)\n\
    \    "
    [Dt.HeapPointerF (Dt.StrPointerF ("tree", [||]))]

let test2 () =
  testMachineNoTc
    "\n  animal(X) :- cat(X).\n  cat(fluffy).\n\n  ?- animal(fluffy)\n  " []

let test3 () =
  testMachineNoTc "\n    animal(dusk).\n    ?- animal(X)\n    "
    [Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))]

let test4 () =
  testMachineNoTc
    "\n\
    \    nextTo(house,car).\n\
    \    nextTo(tree,tree).\n\
    \    ?- nextTo(X,X)\n\
    \    "
    [Dt.HeapPointerF (Dt.StrPointerF ("tree", [||]))]

let test5 () =
  testMachineNoTc
    "\n    animal(dusk).\n    animal(craig).\n    ?- animal(X)\n    "
    [Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))]

let test6 () =
  testMachineNoTc
    "\n   person(craig).\n   dog(dusk).\n   ?- person(X), dog(Y)\n   "
    [ Dt.HeapPointerF (Dt.StrPointerF ("craig", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])) ]

let backtrack () =
  testMachineNoTc
    "\n\
    \    animal(craig).\n\
    \    animal(dusk).\n\
    \    dog(dusk).\n\
    \    ?- animal(X), dog(X)\n\
    \    "
    [Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))]

let doubleVar () =
  testMachineNoTc
    "\n  person(craig).\n  dog(dusk).\n  ?- person(X), dog(Y)\n  "
    [ Dt.HeapPointerF (Dt.StrPointerF ("craig", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])) ]

let swaps1 () =
  testMachineNoTc
    "\n\
    \    g(dusk).\n\
    \    h(craig).\n\n\
    \    f(X,Y) :- g(X), h(Y).\n\n\
    \    ?-f(Y,X)\n\
    \      "
    [ Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("craig", [||])) ]

let noSwaps () =
  testMachineNoTc
    "\n\
    \    g(dusk).\n\
    \    h(craig).\n\n\
    \    f(A,B) :- g(A), h(B).\n\n\
    \    ?-f(Y,X)\n\
    \    "
    [ Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("craig", [||])) ]

let swaps2 () =
  testMachineNoTc
    "\n\
    \    g(dusk).\n\
    \    h(craig).\n\n\
    \    f(X,Y) :- g(Y), h(X).\n\n\
    \    ?-f(Y,X)\n\
    \    "
    [ Dt.HeapPointerF (Dt.StrPointerF ("craig", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])) ]

let buildStruct1 () =
  testMachineNoTc "\n  plant(flower(X)).\n\n  ?- plant(X)\n  "
    [Dt.HeapPointerF (Dt.StrPointerF ("flower", [|Dt.UnboundVarF|]))]

let buildStruct2 () =
  testMachineNoTc "\n    plant(flower(green(X))).\n\n    ?- plant(X)\n    "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ("flower", [|Dt.StrPointerF ("green", [|Dt.UnboundVarF|])|])) ]

let nestedStructures () =
  testMachineNoTc
    "\n    g(dusk).\n    p(X,f(X)) :- g(X).\n    ?- p(X,f(X))\n    "
    [Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))]

let plus1 () =
  testMachineNoTc
    "\n\
    \    add(X,zero,X).\n\
    \    add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\
    \    ?- add(zero,s(zero),X)\n\
    \    "
    [Dt.HeapPointerF (Dt.StrPointerF ("s", [|Dt.StrPointerF ("zero", [||])|]))]

let plus2 () =
  testMachineNoTc
    "\n\
    \    add(X,zero,X).\n\
    \    add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\
    \    ?- add(s(zero),s(zero),X)\n\
    \    "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ("s", [|Dt.StrPointerF ("s", [|Dt.StrPointerF ("zero", [||])|])|]))
    ]

let inStruct () =
  testMachineNoTc "\n    f(X).\n    ?- f(struct(X))\n    "
    [Dt.HeapPointerF Dt.UnboundVarF]

let outStruct () =
  testMachineNoTc "\n  f(struct(X)).\n\n  ?- f(X)\n  "
    [Dt.HeapPointerF (Dt.StrPointerF ("struct", [|Dt.UnboundVarF|]))]

let alpha () =
  testMachineNoTc
    "\n    g(build(X)).\n    f(struct(X)) :- g(X).\n\n    ?- f(X)\n    "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ("struct", [|Dt.StrPointerF ("build", [|Dt.UnboundVarF|])|])) ]

let zeroPLus2 () =
  testMachineNoTc
    "\n\
    \  add(X,zero,X).\n\
    \  add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\n\n\
    \  ?- add(zero,s(s(zero)),X)\n\n\
    \  "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ("s", [|Dt.StrPointerF ("s", [|Dt.StrPointerF ("zero", [||])|])|]))
    ]

let fib2 () =
  testMachineNoTc
    "\n\n\n\
    \    add(X,zero,X).\n\
    \    add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\n\n\
    \    fib(zero,zero).\n\
    \    fib(s(zero),s(zero)).\n\n\
    \    fib(s(s(X)),Y) :- fib(X,A), fib(s(X),B), add(A,B,Y).\n\n\
    \    ?- fib(s(s(zero)),X)\n\n\
    \    "
    [Dt.HeapPointerF (Dt.StrPointerF ("s", [|Dt.StrPointerF ("zero", [||])|]))]

let twop3 () =
  testMachineNoTc
    "\n\
    \  add(X,zero,X).\n\
    \  add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\n\n\
    \  ?- add(s(s(zero)),s(s(s(zero))),s(X))\n\n\
    \  "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "s"
           , [| Dt.StrPointerF
                  ( "s"
                  , [| Dt.StrPointerF
                         ( "s"
                         , [| Dt.StrPointerF
                                ("s", [|Dt.StrPointerF ("zero", [||])|]) |] )
                    |] ) |] )) ]

let addOneFun () =
  testMachineNoTc
    "\n\
    \    add(X,zero,X).\n\
    \    add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\n\
    \    addOne(X,Y) :- add(X,s(zero),A), add(A,zero,Y).\n\n\
    \    ?- addOne(zero,A)\n\
    \    "
    [Dt.HeapPointerF (Dt.StrPointerF ("s", [|Dt.StrPointerF ("zero", [||])|]))]

let fib4 () =
  testMachineNoTc
    "\n\
    \  add(X,zero,X).\n\
    \  add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\n\n\
    \  fib(zero,zero).\n\
    \  fib(s(zero),s(zero)).\n\n\
    \  fib(s(s(X)),Y) :- fib(X,A), fib(s(X),B), add(A,B,Y).\n\n\
    \  ?- fib(s(s(s(s(zero)))),X)\n\
    \  "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "s"
           , [| Dt.StrPointerF
                  ( "s"
                  , [|Dt.StrPointerF ("s", [|Dt.StrPointerF ("zero", [||])|])|]
                  ) |] )) ]

let fib13 () =
  testMachineNoTc
    "\n\
    \  add(X,zero,X).\n\
    \  add(X,s(Y),s(Z)) :- add(X,Y,Z).\n\n\n\
    \  fib(zero,zero).\n\
    \  fib(s(zero),s(zero)).\n\n\
    \  fib(s(s(X)),Y) :- fib(X,A), fib(s(X),B), add(A,B,Y).\n\n\
    \  ?- fib(s(s(s(s(s(s(s(zero))))))),X)\n\
    \  "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "s"
           , [| Dt.StrPointerF
                  ( "s"
                  , [| Dt.StrPointerF
                         ( "s"
                         , [| Dt.StrPointerF
                                ( "s"
                                , [| Dt.StrPointerF
                                       ( "s"
                                       , [| Dt.StrPointerF
                                              ( "s"
                                              , [| Dt.StrPointerF
                                                     ( "s"
                                                     , [| Dt.StrPointerF
                                                            ( "s"
                                                            , [| Dt.StrPointerF
                                                                   ( "s"
                                                                   , [| Dt
                                                                        .StrPointerF
                                                                          ( "s"
                                                                          , [| Dt
                                                                               .StrPointerF
                                                                               ( 
                                                                               "s"
                                                                               , 
                                                                               [| 
                                                                               Dt
                                                                               .StrPointerF
                                                                               ( 
                                                                               "s"
                                                                               , 
                                                                               [| 
                                                                               Dt
                                                                               .StrPointerF
                                                                               ( 
                                                                               "s"
                                                                               , 
                                                                               [| 
                                                                               Dt
                                                                               .StrPointerF
                                                                               ( 
                                                                               "zero"
                                                                               , 
                                                                               [| 
                                                                               |]
                                                                               )
                                                                               |]
                                                                               )
                                                                               |]
                                                                               )
                                                                               |]
                                                                               )
                                                                            |]
                                                                          )
                                                                     |] ) |] )
                                                       |] ) |] ) |] ) |] ) |]
                         ) |] ) |] )) ]

let pairOne () =
  testMachineNoTc
    "\n\
    \      ll(one,two).\n\n\
    \      unif(X,X).\n\n\
    \      less(null,Z, null).\n\
    \      less(pair(H,T), Bound, Result) :- ll(H,Bound), less(T, Bound, \
     NewRes), unif(Result, pair(H, NewRes)).\n\n\
    \      ?- less(pair(one,null),two,Result)\n\
    \      "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "pair"
           , [|Dt.StrPointerF ("one", [||]); Dt.StrPointerF ("null", [||])|] ))
    ]

let cyclicTest () =
  testMachineNoTc
    "\n\
    \    dog(dusk).\n\
    \    unif(X,X).\n\n\
    \    ?-  unif(f(Y),f(X)), dog(X)\n\
    \    "
    [ Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])) ]

let tripleBacktrack () =
  testMachineNoTc
    "\n\
    \    animal(craig).\n\
    \    animal(phoebe).\n\
    \    animal(dusk).\n\
    \    dog(dusk).\n\n\
    \    ?- animal(X), dog(X)\n\
    \    "
    [Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))]

let geqTest () =
  testMachineNoTc
    "\n\
    \    ll(one,two).\n\n\
    \     gg(three,three).\n\n\
    \     unif(X,X).\n\n\
    \     geq(null,Z, null).\n\n\
    \     geq(pair(H,T), Bound, Result) :- gg(H,Bound), geq(T, Bound, \
     NewRes), unif(Result, pair(H, NewRes)).\n\n\
    \     geq(pair(H,T), Bound, Result) :- ll(H, Bound), geq(T, Bound, Result).\n\n\
    \    ?- geq(pair(one,pair(one,null)),two,Res)\n\
    \    "
    [Dt.HeapPointerF (Dt.StrPointerF ("null", [||]))]

let quick21 () =
  testMachineNoTc
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
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "pair"
           , [| Dt.StrPointerF ("one", [||])
              ; Dt.StrPointerF
                  ( "pair"
                  , [| Dt.StrPointerF ("two", [||])
                     ; Dt.StrPointerF ("null", [||]) |] ) |] )) ]

let quick213 () =
  testMachineNoTc
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
    \  part(X, Low, Mid, Up),\n\
    \  quick(Low, ResLow),\n\
    \  quick(Up, ResUp),\n\
    \  join(ResLow, Mid, ResUp, Result).\n\n\n\
    \  ?- quick(pair(two,pair(one,pair(three,null))),X)\n\n\
    \  "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "pair"
           , [| Dt.StrPointerF ("one", [||])
              ; Dt.StrPointerF
                  ( "pair"
                  , [| Dt.StrPointerF ("two", [||])
                     ; Dt.StrPointerF ("three", [||]) |] ) |] )) ]

let sort21312 () =
  testMachineNoTc
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
    \  part(X, Low, Mid, Up),\n\
    \  quick(Low, ResLow),\n\
    \  quick(Up, ResUp),\n\
    \  join(ResLow, Mid, ResUp, Result).\n\n\n\
    \  ?- quick(pair(two,pair(one,pair(three,pair(one,pair(two,null))))),X)\n\n\
    \  "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "pair"
           , [| Dt.StrPointerF ("one", [||])
              ; Dt.StrPointerF
                  ( "pair"
                  , [| Dt.StrPointerF ("one", [||])
                     ; Dt.StrPointerF
                         ( "pair"
                         , [| Dt.StrPointerF ("two", [||])
                            ; Dt.StrPointerF
                                ( "pair"
                                , [| Dt.StrPointerF ("two", [||])
                                   ; Dt.StrPointerF
                                       ( "pair"
                                       , [| Dt.StrPointerF ("three", [||])
                                          ; Dt.StrPointerF ("null", [||]) |] )
                                  |] ) |] ) |] ) |] )) ]

let backtrackDfs () =
  testMachineNoTc
    "\n\
    \    unify(X,X).\n\n\
    \    dfs(V,tree(V,T1,T2),eol).\n\
    \    dfs(V,tree(X,T1,T2),P1) :- dfs(V,T1,P2), unify(P1,cons(left,P2)) .\n\
    \    dfs(V,tree(X,T1,T2),P1) :- dfs(V,T2,P2), unify(P1,cons(right,P2)) .\n\n\
    \    ?- \
     dfs(three,tree(zero,tree(one,tree(five,n,n),tree(six,n,n)),tree(two,tree(three,n,n),tree(four,n,n))),X)\n\
    \    "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "cons"
           , [| Dt.StrPointerF ("right", [||])
              ; Dt.StrPointerF
                  ( "cons"
                  , [| Dt.StrPointerF ("left", [||])
                     ; Dt.StrPointerF ("eol", [||]) |] ) |] )) ]

let backtrackDfs2 () =
  testMachineNoTc
    "\n\
    \    unify(X,X).\n\n\
    \    dfs(V,tree(V,T1,T2),eol).\n\
    \    dfs(V,tree(X,T1,T2),P1) :- dfs(V,T1,P2), unify(P1,cons(left,P2)) .\n\
    \    dfs(V,tree(X,T1,T2),P1) :- dfs(V,T2,P2), unify(P1,cons(right,P2)) .\n\n\
    \    ?- \
     dfs(six,tree(zero,tree(one,tree(five,n,n),tree(six,n,n)),tree(two,tree(three,n,n),tree(four,n,n))),X)\n\
    \    "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "cons"
           , [| Dt.StrPointerF ("left", [||])
              ; Dt.StrPointerF
                  ( "cons"
                  , [| Dt.StrPointerF ("right", [||])
                     ; Dt.StrPointerF ("eol", [||]) |] ) |] )) ]

let testIs () =
  testMachineNoTc "f(X) :- X is 3.\n  ?- f(Y)\n  "
    [Dt.HeapPointerF (Dt.IntF 3)]

let add2 () =
  testMachineNoTc "f(X) :- X is 3 + 4.\n  ?- f(Y)\n  "
    [Dt.HeapPointerF (Dt.IntF 7)]

let add3 () =
  testMachineNoTc "f(X,Y) :- X is 3 + Y.\n  ?- f(X,3)\n  "
    [Dt.HeapPointerF (Dt.IntF 6)]

let neg1 () =
  testMachineNoTc "\n  f(X) :- X is 3 - 1.\n\n  ?- f(Y)\n  "
    [Dt.HeapPointerF (Dt.IntF 2)]

let fibNum () =
  logError (fun m -> m "hereE") ;
  testMachineNoTc
    "\n\
    \  fib(0,0).\n\
    \  fib(1,1).\n\n\
    \  fib(X,Y) :- Arg1 is  X + -2, Arg2 is X + -1, fib(Arg1, Ans1),\n\
    \  fib(Arg2,Ans2), Y is Ans1 + Ans2.\n\n\
    \  ?- fib(9,X)\n\
    \  "
    [Dt.HeapPointerF (Dt.IntF 34)]

let loadArgOrder () =
  testMachineNoTc
    "\n  i(g(h(Z),Z)).\n  f(X) :- i(X).\n  ?-f(g(h(3),X))\n  "
    [Dt.HeapPointerF (Dt.IntF 3)]

let testBigStruct () =
  testMachineNoTc
    "\n  i(g(h(Z),3)).\n  f(X) :- i(g(h(X),X)).\n  ?-f(X)\n  "
    [Dt.HeapPointerF (Dt.IntF 3)]

let anotherTest () =
  testMachineNoTc
    "\n  i(g(h(Z),Z)).\n  f(X) :- i(X).\n  ?-f(g(h(3),X))\n  "
    [Dt.HeapPointerF (Dt.IntF 3)]

let putTest1 () =
  testMachineNoTc "\n  i(g(X,h(3))).\n  ?-i(g(X,h(X)))\n  "
    [Dt.HeapPointerF (Dt.IntF 3)]

let putTest2 () =
  testMachineNoTc "\n  i(g(3,h(X))).\n  ?-i(g(X,h(X)))\n  "
    [Dt.HeapPointerF (Dt.IntF 3)]

let build1 () =
  testMachineNoTc
    "\n\
    \  build(null,0).\n\
    \  build(cons(X),N) :- N1 is N - 1, build(X,N1).\n\n\
    \  ?- build(X,3)\n\
    \  "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "cons"
           , [| Dt.StrPointerF
                  ( "cons"
                  , [| Dt.StrPointerF
                         ("cons", [|Dt.StrPointerF ("null", [||])|]) |] ) |] ))
    ]

let aliasing () =
  testMachineNoTc
    "\n\
    \  p(X,Y) :- q(X,Y), r(X), s(Y).\n\
    \  q(Z,Z).\n\
    \  r(a).\n\
    \  s(A).\n\n\
    \  ?- p(X,Y)\n\n\
    \  "
    [ Dt.HeapPointerF (Dt.StrPointerF ("a", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("a", [||])) ]

let uniffff () =
  testMachineNoTc
    "\n  dog(dusk).\n  unif(X,X).\n\n  ?-  unif(X,f(Y)), dog(Y)\n  "
    [ Dt.HeapPointerF (Dt.StrPointerF ("f", [|Dt.StrPointerF ("dusk", [||])|]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])) ]

let alia3 () =
  testMachineNoTc
    "\n  p(X) :- q(X,X).\n  q(a,Y) :- r(Y).\n  r(a).\n\n  ?- p(X)\n  "
    [Dt.HeapPointerF (Dt.StrPointerF ("a", [||]))]

let mincut () =
  testMachineNoTc
    "num(1).\n\
    \  num(2).\n\n\
    \  equals(X, X).\n\
    \  notone(Y) :- equals(1, Y), !, fail.\n\
    \  notone(Y).\n\n\n\
    \  ?-  num(X), notone(X)"
    [Dt.HeapPointerF (Dt.IntF 2)]

let headdif1 () =
  testMachineNoTc
    "\n\
    \  take(c(H,T),H,T).\n\
    \  take(c(H,T),R,c(H,S)) :- take(T,R,S).\n\n\
    \  perm(eol,eol).\n\
    \  perm(X,c(H,T)) :- take(X,H,R), perm(R,T).\n\n\
    \  equals(X, X).\n\
    \  notequals(X, Y) :- equals(X, Y), !, fail.\n\
    \  notequals(X, Y).\n\n\n\
    \  checkHeadDiffNot1(c(Q1,c(Q2,T))) :-\n\
    \  Dist2 is Q1 + 1,\n\
    \  notequals(Q2, Dist2).\n\n\
    \  test :- perm(c(1,c(2,c(3,c(4,eol)))),R).\n\n\
    \  ?-  perm(c(1,c(2,eol)),R), checkHeadDiffNot1(R)\n\n\
    \  "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "c"
           , [| Dt.IntF 2
              ; Dt.StrPointerF
                  ("c", [|Dt.IntF 1; Dt.StrPointerF ("eol", [||])|]) |] )) ]

let queens8 () =
  testMachineNoTc
    "\n\n\
    \                take(c(H,T),H,T).\n\
    \                  take(c(H,T),R,c(H,S)) :- take(T,R,S).\n\n\
    \                  perm(eol,eol).\n\
    \                  perm(X,c(H,T)) :- take(X,H,R), perm(R,T).\n\n\
    \                  equals(X, X).\n\
    \                  notequals(X, Y) :- equals(X, Y), !, fail.\n\
    \                  notequals(X, Y).\n\n\
    \                  checkPair(X,Y,Dist) :-\n\
    \                      X1 is X + Dist,\n\
    \                      X2 is X - Dist,\n\
    \                  notequals(Y, X1),\n\
    \                      notequals(Y,X2).\n\n\
    \                  checkHead(c(X,eol), Y).\n\n\n\
    \                  checkHead(c(Q1,c(Q2,T)), Dist) :-\n\
    \                  Dist2 is Dist + 1,\n\
    \                  checkPair(Q1,Q2, Dist2),\n\
    \                  checkHead(c(Q1,T), Dist2).\n\n\
    \                  checkDiags(c(X,eol)).\n\
    \                  checkDiags(c(Q1,T)) :- checkHead(c(Q1,T),0), \
     checkDiags(T).\n\n\
    \                  queens8(R) :- \
     perm(c(1,c(2,c(3,c(4,c(5,c(6,c(7,c(8,eol)))))))),R), checkDiags(R).\n\n\
    \          test :- checkDiags( c(1, c(5, c(8, c(6, c(3, c(7, c(2, c(4, \
     eol))))))))).\n\n\n\
    \          unif(X,X).\n\n\
    \          ?-  perm(c(1,c(2,c(3,c(4,c(5,c(6,c(7,c(8,eol)))))))),R), \
     checkDiags(R)\n\n\
    \  "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "c"
           , [| Dt.IntF 1
              ; Dt.StrPointerF
                  ( "c"
                  , [| Dt.IntF 5
                     ; Dt.StrPointerF
                         ( "c"
                         , [| Dt.IntF 8
                            ; Dt.StrPointerF
                                ( "c"
                                , [| Dt.IntF 6
                                   ; Dt.StrPointerF
                                       ( "c"
                                       , [| Dt.IntF 3
                                          ; Dt.StrPointerF
                                              ( "c"
                                              , [| Dt.IntF 7
                                                 ; Dt.StrPointerF
                                                     ( "c"
                                                     , [| Dt.IntF 2
                                                        ; Dt.StrPointerF
                                                            ( "c"
                                                            , [| Dt.IntF 4
                                                               ; Dt.StrPointerF
                                                                   ("eol", [||])
                                                              |] ) |] ) |] )
                                         |] ) |] ) |] ) |] ) |] )) ]

let bigunif () =
  testMachineNoTc
    "\n\
    \      dog(dusk).\n\
    \      unif(X,X).\n\n\
    \      ?-  unif(A,B), unif(C,D), unif(B,C), dog(A)"
    [ Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])) ]

let unif1 () =
  testMachineNoTc
    "unif(X,X).\n\
    \  dog(dusk).\n\n\
    \  ?- unif(A,B), unif(C,D), unif(A,D), dog(A)"
    [ Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])) ]

let unif2 () =
  testMachineNoTc
    "unif(X,X).\n\
    \       dog(dusk).\n\n\
    \       ?- unif(A,B), unif(C,D), unif(A,C), dog(A)"
    [ Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])) ]

let unif3 () =
  testMachineNoTc
    "unif(X,X).\n\
    \            dog(dusk).\n\n\
    \            ?- unif(A,B), unif(C,D), unif(A,C), dog(C)"
    [ Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])) ]

let unifStructs () =
  testMachineNoTc
    "\n\n\n\
    \  unif(X,X).\n\
    \  dog(dusk).\n\n\
    \  ?- unif(f(A),f(B)), unif(f(C),D), unif(f(A),f(C)), dog(A)\n\
    \  "
    [ Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||]))
    ; Dt.HeapPointerF (Dt.StrPointerF ("f", [|Dt.StrPointerF ("dusk", [||])|]))
    ]

let miniperm () =
  testMachineNoTc
    "\n\
    \      take(c(H,T),H,T).\n\
    \      take(c(H,T),R,c(H,S)) :- take(T,R,S).\n\n\
    \      perm(eol,eol).\n\
    \      perm(X,c(H,T)) :- take(X,H,R), perm(R,T).\n\n\
    \      equals(X, X).\n\n\
    \      ?-  perm(c(1,c(2,eol)),R), equals(R,c(2,c(1,eol)))\n\n\
    \  "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "c"
           , [| Dt.IntF 2
              ; Dt.StrPointerF
                  ("c", [|Dt.IntF 1; Dt.StrPointerF ("eol", [||])|]) |] )) ]

let minitree () =
  testMachineNoTc
    "\n\
    \  unify(X,X).\n\n\
    \  dfs(V,tree(V,T1,T2),eol).\n\
    \  dfs(V,tree(X,T1,T2),P1) :- dfs(V,T1,P2), unify(P1,cons(left,P2)) .\n\
    \  dfs(V,tree(X,T1,T2),P1) :- dfs(V,T2,P2), unify(P1,cons(right,P2)) .\n\n\
    \  ?- dfs(2,tree(0,tree(1,n,n),tree(2,n,n)),X)\n\
    \  "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "cons"
           , [|Dt.StrPointerF ("right", [||]); Dt.StrPointerF ("eol", [||])|]
           )) ]

let listy () =
  testMachineNoTc
    "\n\n  dfs(V,tree(V),eol).\n\n  ?- dfs(two,tree(two),X)\n  "
    [Dt.HeapPointerF (Dt.StrPointerF ("eol", [||]))]

let rightright () =
  testMachineNoTc
    "\n\
    \  unify(X,X).\n\n\
    \  dfs(V,tree(V,T1,T2),eol).\n\
    \  dfs(V,tree(X,T1,T2),P1) :- dfs(V,T2,P2), unify(P1,cons(right,P2)) .\n\n\
    \  ?- dfs(three,tree(zero,n,tree(two,n,tree(three,n,n))),X)\n\
    \  "
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "cons"
           , [| Dt.StrPointerF ("right", [||])
              ; Dt.StrPointerF
                  ( "cons"
                  , [| Dt.StrPointerF ("right", [||])
                     ; Dt.StrPointerF ("eol", [||]) |] ) |] )) ]

let testgetint () =
  testMachineNoTc
    "    p(X,Y) :- q(X,Y), r(X), s(Y).\n\
    \    q(Z,Z).\n\
    \    r(1).\n\
    \    s(X).\n\n\
    \    ?- p(X,Y)"
    [Dt.HeapPointerF (Dt.IntF 1); Dt.HeapPointerF (Dt.IntF 1)]

let clearT () =
  testMachineNoTc
    "\n\
     iter(0).\n\n\
     iter(N) :- N1 is N - 1, N2 is N - 2, iter(N1).\n\n\n\
     halt.\n\n\
     goal :- iter(2), halt.\n\n\n\
     ?- goal\n\n\n\n\n"
    []

let cutInStack () =
  testMachineNoTc
    "a(1).\n\
     a(2).\n\n\
     b(2).\n\n\
     g :- a(X),!, b(X).\n\
     f.\n\n\
     res(a) :- g.\n\
     res(b) :- f.\n\n\
     ?- res(X)"
    [Dt.HeapPointerF (Dt.StrPointerF ("b", [||]))]

let crapsort1 () =
  testMachineNoTc
    "    leq(1,1).\n\
    \    leq(1,2).\n\
    \    leq(1,3).\n\
    \    leq(2,2).\n\
    \    leq(2,3).\n\
    \    leq(3,3).\n\n\
    \    take(c(H,T),H,T).\n\
    \    take(c(H,T),R,c(H,S)) :-  take(T,R,S).\n\
    \    take(c(5,T),5,c(5,T)).\n\n\n\
    \    perm(eol,eol).\n\
    \    perm(X,c(H,T)) :- take(X,H,R), perm(R,T).\n\n\
    \    sorted(eol).\n\
    \    sorted(c(H,eol)).\n\
    \    sorted(c(H1,c(H2,T))) :- leq(H1,H2), sorted(c(H2,T)).\n\n\
    \    terribleSort(X,Y) :- perm(c(2,c(3,c(1,c(2,eol)))),A), sorted(Y).\n\n\n\
    \    ?- terribleSort(c(2,c(3,c(1,c(2,eol)))),A)"
    [Dt.HeapPointerF (Dt.StrPointerF ("eol", [||]))]

let crapsort2 () =
  testMachineNoTc
    "    leq(1,1).\n\
    \    leq(1,2).\n\
    \    leq(1,3).\n\
    \    leq(2,2).\n\
    \    leq(2,3).\n\
    \    leq(3,3).\n\n\
    \    take(c(H,T),H,T).\n\
    \    take(c(H,T),R,c(H,S)) :- !,take(T,R,S),leq(1,1).\n\n\n\
    \    perm(eol,eol).\n\
    \    perm(X,c(H,T)) :- take(X,H,R), perm(R,T),leq(1,1).\n\n\n\
    \    sorted(eol).\n\
    \    sorted(c(H,eol)).\n\
    \    sorted(c(H1,c(H2,T))) :- leq(H1,H2), sorted(c(H2,T)),leq(1,1).\n\n\
    \    terribleSort(X,Y) :- perm(X,Y), sorted(Y),leq(1,1).\n\n\n\
    \    ?- terribleSort(c(2,c(3,c(1,c(2,eol)))),A)"
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "c"
           , [| Dt.IntF 1
              ; Dt.StrPointerF
                  ( "c"
                  , [| Dt.IntF 2
                     ; Dt.StrPointerF
                         ( "c"
                         , [| Dt.IntF 2
                            ; Dt.StrPointerF
                                ( "c"
                                , [|Dt.IntF 3; Dt.StrPointerF ("eol", [||])|]
                                ) |] ) |] ) |] )) ]

let tests_notc =
  [ "test1" >:: test1
  ; "test2" >:: test2
  ; "test3" >:: test3
  ; "test4" >:: test4
  ; "test6" >:: test6
  ; "backtrack" >:: backtrack
  ; "doubleVar" >:: doubleVar
  ; "swaps1" >:: swaps1
  ; "noSwaps" >:: noSwaps
  ; "swaps2" >:: swaps2
  ; "buildStruct1" >:: buildStruct1
  ; "buildStruct2" >:: buildStruct2
  ; "nestedStructures" >:: nestedStructures
  ; "plus1" >:: plus1
  ; "plus2" >:: plus2
  ; "inStruct" >:: inStruct
  ; "alpha" >:: alpha
  ; "zeroPLus2" >:: zeroPLus2
  ; "fib2" >:: fib2
  ; "twop3" >:: twop3
  ; "addOneFun" >:: addOneFun
  ; "fib4" >:: fib4
  ; "fib13" >:: fib13
  ; "pairOne" >:: pairOne
  ; "cyclicTest" >:: cyclicTest
  ; "tripleBacktrack" >:: tripleBacktrack
  ; "geqTest" >:: geqTest
  ; "quick21" >:: quick21
  ; "quick213" >:: quick213
  ; "sort21312" >:: sort21312
  ; "backtrackDfs" >:: backtrackDfs
  ; "backtrackDfs2" >:: backtrackDfs2
  ; "testIs" >:: testIs
  ; "add2" >:: add2
  ; "add3" >:: add3
  ; "neg1" >:: neg1
  ; "loadArgOrder" >:: loadArgOrder
  ; "testBigStruct" >:: testBigStruct
  ; "anotherTest" >:: anotherTest
  ; "putTest1" >:: putTest1
  ; "putTest2" >:: putTest2
  ; "build1" >:: build1
  ; "aliasing" >:: aliasing
  ; "uniffff" >:: uniffff
  ; "alia3" >:: alia3
  ; "mincut" >:: mincut
  ; "headdif1" >:: headdif1
  ; "queens8" >:: queens8
  ; "bigunif" >:: bigunif
  ; "unif1" >:: unif1
  ; "unif2" >:: unif2
  ; "unif3" >:: unif3
  ; "unifStructs" >:: unifStructs
  ; "miniperm" >:: miniperm
  ; "listy" >:: listy
  ; "rightright" >:: rightright
  ; "testgetint" >:: testgetint
  ; "fibNum" >:: fibNum
  ; "clearT" >:: clearT
  ; "cutInStack" >:: cutInStack
  ; "crapsort1" >:: crapsort1
  ; "crapsort2" >:: crapsort2 ]
