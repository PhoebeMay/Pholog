
open OUnit
open Dt
open Runall
open Logging

let testMachineNoTc input_str expected_result =
  let res = (execute input_str false)
  in match res with
    Some(r) ->
    assert_equal r expected_result
  | None -> raise Oops



let test1 () =
  testMachineNoTc
    "
    nextTo(house,car).
    nextTo(tree,tree).
    ?- nextTo(X,X)
    "
    ([(Dt.HeapPointerF ((Dt.StrPointerF ("tree", [||]))))]
    )

let test2 () =
  testMachineNoTc
    "
  animal(X) :- cat(X).
  cat(fluffy).

  ?- animal(fluffy)
  "
    []

let test3 () =
  testMachineNoTc
    "
    animal(dusk).
    ?- animal(X)
    "
    [(Dt.HeapPointerF ( (Dt.StrPointerF ("dusk", [||]))))]


let test4 () =
  testMachineNoTc
    "
    nextTo(house,car).
    nextTo(tree,tree).
    ?- nextTo(X,X)
    "
    [(Dt.HeapPointerF ((Dt.StrPointerF ("tree", [||]))))]


let test5 () =
  testMachineNoTc
    "
    animal(dusk).
    animal(craig).
    ?- animal(X)
    "
    [(Dt.HeapPointerF ( (Dt.StrPointerF ("dusk", [||]))))]


let test6 () =
  testMachineNoTc
    "
   person(craig).
   dog(dusk).
   ?- person(X), dog(Y)
   "
    [(Dt.HeapPointerF ( (Dt.StrPointerF ("craig", [||]))));
     (Dt.HeapPointerF ( (Dt.StrPointerF ("dusk", [||]))))]

let backtrack () =
  testMachineNoTc
    "
    animal(craig).
    animal(dusk).
    dog(dusk).
    ?- animal(X), dog(X)
    "
    [(Dt.HeapPointerF ( (Dt.StrPointerF ("dusk", [||]))))]

let doubleVar () =
  testMachineNoTc
    "
  person(craig).
  dog(dusk).
  ?- person(X), dog(Y)
  "
    [(Dt.HeapPointerF ( (Dt.StrPointerF ("craig", [||]))));
     (Dt.HeapPointerF ( (Dt.StrPointerF ("dusk", [||]))))]


let swaps1 () =
  testMachineNoTc
    "
    g(dusk).
    h(craig).

    f(X,Y) :- g(X), h(Y).

    ?-f(Y,X)
      "
    [(Dt.HeapPointerF ( (Dt.StrPointerF ("dusk", [||]))));
     (Dt.HeapPointerF ( (Dt.StrPointerF ("craig", [||]))))]


let noSwaps () =
  testMachineNoTc
    "
    g(dusk).
    h(craig).

    f(A,B) :- g(A), h(B).

    ?-f(Y,X)
    "
    [(Dt.HeapPointerF ( (Dt.StrPointerF ("dusk", [||]))));
     (Dt.HeapPointerF ( (Dt.StrPointerF ("craig", [||]))))]

let swaps2 () =
  testMachineNoTc
    "
    g(dusk).
    h(craig).

    f(X,Y) :- g(Y), h(X).

    ?-f(Y,X)
    "
    [(Dt.HeapPointerF ( (Dt.StrPointerF ("craig", [||]))));
     (Dt.HeapPointerF ( (Dt.StrPointerF ("dusk", [||]))))]

let buildStruct1 () =
  testMachineNoTc
    "
  plant(flower(X)).

  ?- plant(X)
  "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("flower", [|( Dt.UnboundVarF)|]))))
    ]



let buildStruct2 () =
  testMachineNoTc
    "
    plant(flower(green(X))).

    ?- plant(X)
    "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("flower",
                           [|(
                             (Dt.StrPointerF ("green",
                                              [|( Dt.UnboundVarF)|])))
                           |]
                          ))))
    ]



let nestedStructures () =
  testMachineNoTc
    "
    g(dusk).
    p(X,f(X)) :- g(X).
    ?- p(X,f(X))
    "
    [(Dt.HeapPointerF ( (Dt.StrPointerF ("dusk", [||]))))]


let plus1 () =
  testMachineNoTc
    "
    add(X,zero,X).
    add(X,s(Y),s(Z)) :- add(X,Y,Z).
    ?- add(zero,s(zero),X)
    "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("s",
                           [|( (Dt.StrPointerF ("zero", [||])))|]))))
    ]


let plus2 () =
  testMachineNoTc
    "
    add(X,zero,X).
    add(X,s(Y),s(Z)) :- add(X,Y,Z).
    ?- add(s(zero),s(zero),X)
    "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("s",
                           [|(
                             (Dt.StrPointerF ("s",
                                              [|( (Dt.StrPointerF ("zero", [||])))|])))
                           |]
                          ))))
    ]


let inStruct () =
  testMachineNoTc
    "
    f(X).
    ?- f(struct(X))
    "
    [(Dt.HeapPointerF ( Dt.UnboundVarF))]

let outStruct () =
  testMachineNoTc
    "
  f(struct(X)).

  ?- f(X)
  "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("struct", [|( Dt.UnboundVarF)|]))))
    ]

let alpha () =
  testMachineNoTc
    "
    g(build(X)).
    f(struct(X)) :- g(X).

    ?- f(X)
    "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("struct",
                           [|(
                             (Dt.StrPointerF ("build",
                                              [|( Dt.UnboundVarF)|])))
                           |]
                          ))))
    ]


let zeroPLus2 () = testMachineNoTc
    "
  add(X,zero,X).
  add(X,s(Y),s(Z)) :- add(X,Y,Z).


  ?- add(zero,s(s(zero)),X)

  "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("s",
                           [|(
                             (Dt.StrPointerF ("s",
                                              [|( (Dt.StrPointerF ("zero", [||])))|])))
                           |]
                          ))))
    ]


let fib2 () =
  testMachineNoTc
    "


    add(X,zero,X).
    add(X,s(Y),s(Z)) :- add(X,Y,Z).


    fib(zero,zero).
    fib(s(zero),s(zero)).

    fib(s(s(X)),Y) :- fib(X,A), fib(s(X),B), add(A,B,Y).

    ?- fib(s(s(zero)),X)

    "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("s",
                           [|( (Dt.StrPointerF ("zero", [||])))|]))))
    ]


let twop3 () =
  testMachineNoTc
    "
  add(X,zero,X).
  add(X,s(Y),s(Z)) :- add(X,Y,Z).


  ?- add(s(s(zero)),s(s(s(zero))),s(X))

  "[(Dt.HeapPointerF
     (
       (Dt.StrPointerF ("s",
                        [|(
                          (Dt.StrPointerF ("s",
                                           [|(
                                             (Dt.StrPointerF ("s",
                                                              [|(
                                                                (Dt.StrPointerF ("s",
                                                                                 [|(
                                                                                   (Dt.StrPointerF ("zero",
                                                                                                    [||])))
                                                                                 |]
                                                                                )))
                                                              |]
                                                             )))
                                           |]
                                          )))
                        |]
                       ))))
 ]


let addOneFun () =
  testMachineNoTc
    "
    add(X,zero,X).
    add(X,s(Y),s(Z)) :- add(X,Y,Z).

    addOne(X,Y) :- add(X,s(zero),A), add(A,zero,Y).

    ?- addOne(zero,A)
    "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("s",
                           [|( (Dt.StrPointerF ("zero", [||])))|]))))
    ]

let fib4 () = testMachineNoTc
    "
  add(X,zero,X).
  add(X,s(Y),s(Z)) :- add(X,Y,Z).


  fib(zero,zero).
  fib(s(zero),s(zero)).

  fib(s(s(X)),Y) :- fib(X,A), fib(s(X),B), add(A,B,Y).

  ?- fib(s(s(s(s(zero)))),X)
  "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("s",[|((Dt.StrPointerF ("s",[|((Dt.StrPointerF ("s",[|( (Dt.StrPointerF ("zero", [||])))|])))|])))|] ))))
    ]


let fib13 () =
  testMachineNoTc
    "
  add(X,zero,X).
  add(X,s(Y),s(Z)) :- add(X,Y,Z).


  fib(zero,zero).
  fib(s(zero),s(zero)).

  fib(s(s(X)),Y) :- fib(X,A), fib(s(X),B), add(A,B,Y).

  ?- fib(s(s(s(s(s(s(s(zero))))))),X)
  "
    ([(Dt.HeapPointerF
         (
           (Dt.StrPointerF ("s",[|(
                (Dt.StrPointerF ("s",           [|(          (Dt.StrPointerF ("s",                             [|(  (Dt.StrPointerF ("s",                         [|(                                                                                  (Dt.StrPointerF ("s",                                                                                           [|( (Dt.StrPointerF ("s",  [|( (Dt.StrPointerF (  "s",      [|( (Dt.StrPointerF ("s", [|(     (Dt.StrPointerF (   "s",  [|(   (Dt.StrPointerF (     "s", [|( (
                     Dt.StrPointerF ("s", [|(
                         (Dt.StrPointerF ( "s",[|((Dt.StrPointerF ("s",
                                                                   [|(

                                                                     (Dt.StrPointerF (
                                                                         "zero",
                                                                         [||])))|]
                                                                  )))|])))
                       |])))|])))
                   |])))|]
                   )))
                   |]
                   )))|] ))) |] )))
                   |] ))) |]
                   )))   |]
                   )))
              |]
              ))))
     ]

    )

let pairOne () = testMachineNoTc
    "
      ll(one,two).

      unif(X,X).

      less(null,Z, null).
      less(pair(H,T), Bound, Result) :- ll(H,Bound), less(T, Bound, NewRes), unif(Result, pair(H, NewRes)).

      ?- less(pair(one,null),two,Result)
      "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("pair",
                           [|( (Dt.StrPointerF ("one", [||])));
                             ( (Dt.StrPointerF ("null", [||])))|]
                          ))))
    ]



let cyclicTest () =
  testMachineNoTc
    "
    dog(dusk).
    unif(X,X).

    ?-  unif(f(Y),f(X)), dog(X)
    "
    [(Dt.HeapPointerF ( (Dt.StrPointerF ("dusk", [||]))));
     (Dt.HeapPointerF ( (Dt.StrPointerF ("dusk", [||]))))]



let tripleBacktrack ()  =
  testMachineNoTc
    "
    animal(craig).
    animal(phoebe).
    animal(dusk).
    dog(dusk).

    ?- animal(X), dog(X)
    "
    [(Dt.HeapPointerF ( (Dt.StrPointerF ("dusk", [||]))))]

let geqTest () =
  testMachineNoTc
    "
    ll(one,two).

     gg(three,three).

     unif(X,X).

     geq(null,Z, null).

     geq(pair(H,T), Bound, Result) :- gg(H,Bound), geq(T, Bound, NewRes), unif(Result, pair(H, NewRes)).

     geq(pair(H,T), Bound, Result) :- ll(H, Bound), geq(T, Bound, Result).

    ?- geq(pair(one,pair(one,null)),two,Res)
    "
    [(Dt.HeapPointerF ( (Dt.StrPointerF ("null", [||]))))]

let quick21 () =
  testMachineNoTc
    "
  ll(one,three).
  ll(one,two).

  ll(two,three).

  geq(one,one).

  geq(two,one).
  geq(two,two).

  geq(three,one).
  geq(three,two).
  geq(three,three).

  unif(X,X).

  less(null,Z, null).
  less(pair(H,T), Bound, Result) :- ll(H,Bound), less(T, Bound, NewRes), unif(Result, pair(H, NewRes)).
  less(pair(H,T), Bound, Result) :- geq(H,Bound), less(T, Bound, Result).

  geq(null,Z, null).
  geq(pair(H,T), Bound, Result) :- geq(H,Bound), geq(T, Bound, NewRes), unif(Result, pair(H, NewRes)).
  geq(pair(H,T), Bound, Result) :- ll(H, Bound), geq(T, Bound, Result).

  part(pair(H,T), Low, Mid, Up) :- unif(Mid, H), less(T,H, Low), geq(T,H, Up).

  join(null,Mid, Up, Result) :- unif(Result, pair(Mid,Up)).
  join(pair(H,T),Mid, Up, Result) :- join(T, Mid, Up, NewRes), unif(Result, pair(H,NewRes)).

  quick(null, null).
  quick(pair(X,null), X).
  quick(pair(X, pair(Y, null)), Result) :- geq(Y, X), unif(Result, pair(X, pair(Y, null))).

  quick(X, Result) :-
    part(X, Low, Mid, Up),
    quick(Low, ResLow),
    quick(Up, ResUp),
    join(ResLow, Mid, ResUp, Result).


  ?- quick(pair(two,pair(one,null)),X)

  "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("pair",
                           [|( (Dt.StrPointerF ("one", [||])));
                             (
                               (Dt.StrPointerF ("pair",
                                                [|( (Dt.StrPointerF ("two", [||])));
                                                  ( (Dt.StrPointerF ("null", [||])))|]
                                               )))
                           |]
                          ))))
    ]


let quick213 () = testMachineNoTc
    "
  ll(one,three).
  ll(one,two).

  ll(two,three).

  geq(one,one).

  geq(two,one).
  geq(two,two).

  geq(three,one).
  geq(three,two).
  geq(three,three).

  unif(X,X).

  less(null,Z, null).
  less(pair(H,T), Bound, Result) :- ll(H,Bound), less(T, Bound, NewRes), unif(Result, pair(H, NewRes)).
  less(pair(H,T), Bound, Result) :- geq(H,Bound), less(T, Bound, Result).

  geq(null,Z, null).
  geq(pair(H,T), Bound, Result) :- geq(H,Bound), geq(T, Bound, NewRes), unif(Result, pair(H, NewRes)).
  geq(pair(H,T), Bound, Result) :- ll(H, Bound), geq(T, Bound, Result).

  part(pair(H,T), Low, Mid, Up) :- unif(Mid, H), less(T,H, Low), geq(T,H, Up).

  join(null,Mid, Up, Result) :- unif(Result, pair(Mid,Up)).
  join(pair(H,T),Mid, Up, Result) :- join(T, Mid, Up, NewRes), unif(Result, pair(H,NewRes)).

  quick(null, null).
  quick(pair(X,null), X).
  quick(pair(X, pair(Y, null)), Result) :- geq(Y, X), unif(Result, pair(X, pair(Y, null))).

  quick(X, Result) :-
  part(X, Low, Mid, Up),
  quick(Low, ResLow),
  quick(Up, ResUp),
  join(ResLow, Mid, ResUp, Result).


  ?- quick(pair(two,pair(one,pair(three,null))),X)

  "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("pair",
                           [|( (Dt.StrPointerF ("one", [||])));
                             (
                               (Dt.StrPointerF ("pair",
                                                [|( (Dt.StrPointerF ("two", [||])));
                                                  ( (Dt.StrPointerF ("three", [||])))|]
                                               )))
                           |]
                          ))))
    ]


let sort21312 () = testMachineNoTc
    "
  ll(one,three).
  ll(one,two).

  ll(two,three).

  geq(one,one).

  geq(two,one).
  geq(two,two).

  geq(three,one).
  geq(three,two).
  geq(three,three).

  unif(X,X).

  less(null,Z, null).
  less(pair(H,T), Bound, Result) :- ll(H,Bound), less(T, Bound, NewRes), unif(Result, pair(H, NewRes)).
  less(pair(H,T), Bound, Result) :- geq(H,Bound), less(T, Bound, Result).

  geq(null,Z, null).
  geq(pair(H,T), Bound, Result) :- geq(H,Bound), geq(T, Bound, NewRes), unif(Result, pair(H, NewRes)).
  geq(pair(H,T), Bound, Result) :- ll(H, Bound), geq(T, Bound, Result).

  part(pair(H,T), Low, Mid, Up) :- unif(Mid, H), less(T,H, Low), geq(T,H, Up).

  join(null,Mid, Up, Result) :- unif(Result, pair(Mid,Up)).
  join(pair(H,T),Mid, Up, Result) :- join(T, Mid, Up, NewRes), unif(Result, pair(H,NewRes)).

  quick(null, null).
  quick(pair(X,null), X).
  quick(pair(X, pair(Y, null)), Result) :- geq(Y, X), unif(Result, pair(X, pair(Y, null))).

  quick(X, Result) :-
  part(X, Low, Mid, Up),
  quick(Low, ResLow),
  quick(Up, ResUp),
  join(ResLow, Mid, ResUp, Result).


  ?- quick(pair(two,pair(one,pair(three,pair(one,pair(two,null))))),X)

  "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("pair",
                           [|( (Dt.StrPointerF ("one", [||])));
                             (
                               (Dt.StrPointerF ("pair",
                                                [|( (Dt.StrPointerF ("one", [||])));
                                                  (
                                                    (Dt.StrPointerF ("pair",
                                                                     [|( (Dt.StrPointerF ("two", [||])));
                                                                       (
                                                                         (Dt.StrPointerF ("pair",
                                                                                          [|(
                                                                                            (Dt.StrPointerF ("two", [||])));
                                                                                            (
                                                                                              (Dt.StrPointerF ("pair",
                                                                                                               [|(
                                                                                                                 (Dt.StrPointerF (
                                                                                                                     "three", [||])));
                                                                                                                 (
                                                                                                                   (Dt.StrPointerF (
                                                                                                                       "null", [||])))
                                                                                                               |]
                                                                                                              )))
                                                                                          |]
                                                                                         )))
                                                                     |]
                                                                    )))
                                                |]
                                               )))
                           |]
                          ))))
    ]



let backtrackDfs () =
  testMachineNoTc
    "
    unify(X,X).

    dfs(V,tree(V,T1,T2),eol).
    dfs(V,tree(X,T1,T2),P1) :- dfs(V,T1,P2), unify(P1,cons(left,P2)) .
    dfs(V,tree(X,T1,T2),P1) :- dfs(V,T2,P2), unify(P1,cons(right,P2)) .

    ?- dfs(three,tree(zero,tree(one,tree(five,n,n),tree(six,n,n)),tree(two,tree(three,n,n),tree(four,n,n))),X)
    "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("cons",
                           [|( (Dt.StrPointerF ("right", [||])));
                             (
                               (Dt.StrPointerF ("cons",
                                                [|( (Dt.StrPointerF ("left", [||])));
                                                  ( (Dt.StrPointerF ("eol", [||])))|]
                                               )))
                           |]
                          ))))
    ]


let backtrackDfs2 () =
  testMachineNoTc
    "
    unify(X,X).

    dfs(V,tree(V,T1,T2),eol).
    dfs(V,tree(X,T1,T2),P1) :- dfs(V,T1,P2), unify(P1,cons(left,P2)) .
    dfs(V,tree(X,T1,T2),P1) :- dfs(V,T2,P2), unify(P1,cons(right,P2)) .

    ?- dfs(six,tree(zero,tree(one,tree(five,n,n),tree(six,n,n)),tree(two,tree(three,n,n),tree(four,n,n))),X)
    "[(Dt.HeapPointerF
       (
         (Dt.StrPointerF ("cons",
                          [|( (Dt.StrPointerF ("left", [||])));
                            (
                              (Dt.StrPointerF ("cons",
                                               [|( (Dt.StrPointerF ("right", [||])));
                                                 ( (Dt.StrPointerF ("eol", [||])))|]
                                              )))
                          |]
                         ))))
   ]


let testIs () = testMachineNoTc
    "f(X) :- X is 3.
  ?- f(Y)
  "
    [(Dt.HeapPointerF ( (Dt.IntF 3)))]

let add2 () = testMachineNoTc
    "f(X) :- X is 3 + 4.
  ?- f(Y)
  "
    [(Dt.HeapPointerF ( (Dt.IntF 7)))]

let add3 () = testMachineNoTc
    "f(X,Y) :- X is 3 + Y.
  ?- f(X,3)
  "
    [(Dt.HeapPointerF ( (Dt.IntF 6)))]

let neg1 () = testMachineNoTc
    "
  f(X) :- X is 3 - 1.

  ?- f(Y)
  "
    [(Dt.HeapPointerF ( (Dt.IntF 2)))]

let fibNum () = logError(fun m -> m "hereE"); testMachineNoTc
    "
  fib(0,0).
  fib(1,1).

  fib(X,Y) :- Arg1 is  X + -2, Arg2 is X + -1, fib(Arg1, Ans1),
  fib(Arg2,Ans2), Y is Ans1 + Ans2.

  ?- fib(9,X)
  "
    [(Dt.HeapPointerF ( (Dt.IntF 34)))]

let loadArgOrder () = testMachineNoTc
    "
  i(g(h(Z),Z)).
  f(X) :- i(X).
  ?-f(g(h(3),X))
  "
    [(Dt.HeapPointerF ( (Dt.IntF 3)))]

let testBigStruct () = testMachineNoTc
    "
  i(g(h(Z),3)).
  f(X) :- i(g(h(X),X)).
  ?-f(X)
  "
    [(Dt.HeapPointerF ( (Dt.IntF 3)))]

let anotherTest () = testMachineNoTc
    "
  i(g(h(Z),Z)).
  f(X) :- i(X).
  ?-f(g(h(3),X))
  "
    [(Dt.HeapPointerF ( (Dt.IntF 3)))]


let putTest1 () = testMachineNoTc
    "
  i(g(X,h(3))).
  ?-i(g(X,h(X)))
  "
    [(Dt.HeapPointerF ( (Dt.IntF 3)))]

let putTest2 () = testMachineNoTc
    "
  i(g(3,h(X))).
  ?-i(g(X,h(X)))
  "
    [(Dt.HeapPointerF ( (Dt.IntF 3)))]


let build1 () = testMachineNoTc
    "
  build(null,0).
  build(cons(X),N) :- N1 is N - 1, build(X,N1).

  ?- build(X,3)
  "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("cons",
                           [|(
                             (Dt.StrPointerF ("cons",
                                              [|(
                                                (Dt.StrPointerF ("cons",
                                                                 [|( (Dt.StrPointerF ("null", [||])))|]
                                                                )))
                                              |]
                                             )))
                           |]
                          ))))
    ]

let aliasing () = testMachineNoTc
    "
  p(X,Y) :- q(X,Y), r(X), s(Y).
  q(Z,Z).
  r(a).
  s(A).

  ?- p(X,Y)

  "
    [(Dt.HeapPointerF ( (Dt.StrPointerF ("a", [||]))));
     (Dt.HeapPointerF ( (Dt.StrPointerF ("a", [||]))))]


let uniffff () = testMachineNoTc
    "
  dog(dusk).
  unif(X,X).

  ?-  unif(X,f(Y)), dog(Y)
  "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("f",
                           [|( (Dt.StrPointerF ("dusk", [||])))|]))));
     (Dt.HeapPointerF ( (Dt.StrPointerF ("dusk", [||]))))]


let alia3 () = testMachineNoTc
    "
  p(X) :- q(X,X).
  q(a,Y) :- r(Y).
  r(a).

  ?- p(X)
  "
    [(Dt.HeapPointerF ( (Dt.StrPointerF ("a", [||]))))]


let mincut () = testMachineNoTc
    "num(1).
  num(2).

  equals(X, X).
  notone(Y) :- equals(1, Y), !, fail.
  notone(Y).


  ?-  num(X), notone(X)"
    [(Dt.HeapPointerF ( (Dt.IntF 2)))]


let headdif1 () = testMachineNoTc
    "
  take(c(H,T),H,T).
  take(c(H,T),R,c(H,S)) :- take(T,R,S).

  perm(eol,eol).
  perm(X,c(H,T)) :- take(X,H,R), perm(R,T).

  equals(X, X).
  notequals(X, Y) :- equals(X, Y), !, fail.
  notequals(X, Y).


  checkHeadDiffNot1(c(Q1,c(Q2,T))) :-
  Dist2 is Q1 + 1,
  notequals(Q2, Dist2).

  test :- perm(c(1,c(2,c(3,c(4,eol)))),R).

  ?-  perm(c(1,c(2,eol)),R), checkHeadDiffNot1(R)

  "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("c",
                           [|( (Dt.IntF 2));
                             (
                               (Dt.StrPointerF ("c",
                                                [|( (Dt.IntF 1));
                                                  ( (Dt.StrPointerF ("eol", [||])))|]
                                               )))
                           |]
                          ))))
    ]


let queens8 () = testMachineNoTc
    "

                take(c(H,T),H,T).
                  take(c(H,T),R,c(H,S)) :- take(T,R,S).

                  perm(eol,eol).
                  perm(X,c(H,T)) :- take(X,H,R), perm(R,T).

                  equals(X, X).
                  notequals(X, Y) :- equals(X, Y), !, fail.
                  notequals(X, Y).

                  checkPair(X,Y,Dist) :-
                      X1 is X + Dist,
                      X2 is X - Dist,
                  notequals(Y, X1),
                      notequals(Y,X2).

                  checkHead(c(X,eol), Y).


                  checkHead(c(Q1,c(Q2,T)), Dist) :-
                  Dist2 is Dist + 1,
                  checkPair(Q1,Q2, Dist2),
                  checkHead(c(Q1,T), Dist2).

                  checkDiags(c(X,eol)).
                  checkDiags(c(Q1,T)) :- checkHead(c(Q1,T),0), checkDiags(T).

                  queens8(R) :- perm(c(1,c(2,c(3,c(4,c(5,c(6,c(7,c(8,eol)))))))),R), checkDiags(R).

          test :- checkDiags( c(1, c(5, c(8, c(6, c(3, c(7, c(2, c(4, eol))))))))).


          unif(X,X).

          ?-  perm(c(1,c(2,c(3,c(4,c(5,c(6,c(7,c(8,eol)))))))),R), checkDiags(R)

  "
    [(Dt.HeapPointerF
        (
          (Dt.StrPointerF ("c",
                           [|( (Dt.IntF 1));
                             (
                               (Dt.StrPointerF ("c",
                                                [|( (Dt.IntF 5));
                                                  (
                                                    (Dt.StrPointerF ("c",
                                                                     [|( (Dt.IntF 8));
                                                                       (
                                                                         (Dt.StrPointerF ("c",
                                                                                          [|( (Dt.IntF 6));
                                                                                            (
                                                                                              (Dt.StrPointerF ("c",
                                                                                                               [|( (Dt.IntF 3));
                                                                                                                 (
                                                                                                                   (Dt.StrPointerF ("c",
                                                                                                                                    [|(
                                                                                                                                      (Dt.IntF 7));
                                                                                                                                      (
                                                                                                                                        (Dt.StrPointerF (
                                                                                                                                            "c",
                                                                                                                                            [|(

                                                                                                                                              (Dt.IntF
                                                                                                                                                 2));
                                                                                                                                              (

                                                                                                                                                (Dt.StrPointerF (
                                                                                                                                                    "c",
                                                                                                                                                    [|(

                                                                                                                                                      (Dt.IntF
                                                                                                                                                         4));
                                                                                                                                                      (
                                                                                                                                                        (Dt.StrPointerF (
                                                                                                                                                            "eol",
                                                                                                                                                            [||])))|]
                                                                                                                                                  )))|]
                                                                                                                                          )))
                                                                                                                                    |]
                                                                                                                                   )))
                                                                                                               |]
                                                                                                              )))
                                                                                          |]
                                                                                         )))
                                                                     |]
                                                                    )))
                                                |]
                                               )))
                           |]
                          ))))
    ]


let bigunif () = testMachineNoTc
    "
      dog(dusk).
      unif(X,X).

      ?-  unif(A,B), unif(C,D), unif(B,C), dog(A)"
    [(Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])));
     (Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])));
     (Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])));
     (Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])))]



let unif1 () = testMachineNoTc
    "unif(X,X).
  dog(dusk).

  ?- unif(A,B), unif(C,D), unif(A,D), dog(A)"
    [(Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])));
     (Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])));
     (Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])));
     (Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])))]

let unif2 () = testMachineNoTc
    "unif(X,X).
       dog(dusk).

       ?- unif(A,B), unif(C,D), unif(A,C), dog(A)"
    [(Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])));
     (Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])));
     (Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])));
     (Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])))]


let unif3 () = testMachineNoTc
    "unif(X,X).
            dog(dusk).

            ?- unif(A,B), unif(C,D), unif(A,C), dog(C)"
    [(Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])));
     (Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])));
     (Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])));
     (Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])))]


let unifStructs () = testMachineNoTc
    "


  unif(X,X).
  dog(dusk).

  ?- unif(f(A),f(B)), unif(f(C),D), unif(f(A),f(C)), dog(A)
  "
    [(Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])));
     (Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])));
     (Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])));
     (Dt.HeapPointerF
        (Dt.StrPointerF ("f", [|(Dt.StrPointerF ("dusk", [||]))|])))
    ]

let miniperm () = testMachineNoTc
    "
      take(c(H,T),H,T).
      take(c(H,T),R,c(H,S)) :- take(T,R,S).

      perm(eol,eol).
      perm(X,c(H,T)) :- take(X,H,R), perm(R,T).

      equals(X, X).

      ?-  perm(c(1,c(2,eol)),R), equals(R,c(2,c(1,eol)))

  "
    [(Dt.HeapPointerF
        (Dt.StrPointerF ("c",
                         [|(Dt.IntF 2);
                           (Dt.StrPointerF ("c",
                                            [|(Dt.IntF 1); (Dt.StrPointerF ("eol", [||]))|]))
                         |]
                        )))
    ]


let minitree () = testMachineNoTc
    "
  unify(X,X).

  dfs(V,tree(V,T1,T2),eol).
  dfs(V,tree(X,T1,T2),P1) :- dfs(V,T1,P2), unify(P1,cons(left,P2)) .
  dfs(V,tree(X,T1,T2),P1) :- dfs(V,T2,P2), unify(P1,cons(right,P2)) .

  ?- dfs(2,tree(0,tree(1,n,n),tree(2,n,n)),X)
  "
    [(Dt.HeapPointerF
        (Dt.StrPointerF ("cons",
                         [|(Dt.StrPointerF ("right", [||]));
                           (Dt.StrPointerF ("eol", [||]))|]
                        )))
    ]


let listy () = testMachineNoTc
    "

  dfs(V,tree(V),eol).

  ?- dfs(two,tree(two),X)
  "
    [(Dt.HeapPointerF (Dt.StrPointerF ("eol", [||])))]

let rightright () = testMachineNoTc
    "
  unify(X,X).

  dfs(V,tree(V,T1,T2),eol).
  dfs(V,tree(X,T1,T2),P1) :- dfs(V,T2,P2), unify(P1,cons(right,P2)) .

  ?- dfs(three,tree(zero,n,tree(two,n,tree(three,n,n))),X)
  "
    [(Dt.HeapPointerF
        (Dt.StrPointerF ("cons",
                         [|(Dt.StrPointerF ("right", [||]));
                           (Dt.StrPointerF ("cons",
                                            [|(Dt.StrPointerF ("right", [||]));
                                              (Dt.StrPointerF ("eol", [||]))|]
                                           ))
                         |]
                        )))
    ]


let testgetint () = testMachineNoTc
    "    p(X,Y) :- q(X,Y), r(X), s(Y).
    q(Z,Z).
    r(1).
    s(X).

    ?- p(X,Y)"
    [(Dt.HeapPointerF (Dt.IntF 1)); (Dt.HeapPointerF (Dt.IntF 1))]


let clearT () = testMachineNoTc
"
iter(0).

iter(N) :- N1 is N - 1, N2 is N - 2, iter(N1).


halt.

goal :- iter(2), halt.


?- goal




"
[]


let cutInStack () = testMachineNoTc
    "a(1).
a(2).

b(2).

g :- a(X),!, b(X).
f.

res(a) :- g.
res(b) :- f.

?- res(X)"
    [(Dt.HeapPointerF (Dt.StrPointerF ("b", [||])))]

let crapsort1 () = testMachineNoTc
    "    leq(1,1).
    leq(1,2).
    leq(1,3).
    leq(2,2).
    leq(2,3).
    leq(3,3).

    take(c(H,T),H,T).
    take(c(H,T),R,c(H,S)) :-  take(T,R,S).
    take(c(5,T),5,c(5,T)).


    perm(eol,eol).
    perm(X,c(H,T)) :- take(X,H,R), perm(R,T).

    sorted(eol).
    sorted(c(H,eol)).
    sorted(c(H1,c(H2,T))) :- leq(H1,H2), sorted(c(H2,T)).

    terribleSort(X,Y) :- perm(c(2,c(3,c(1,c(2,eol)))),A), sorted(Y).


    ?- terribleSort(c(2,c(3,c(1,c(2,eol)))),A)"
    [(Dt.HeapPointerF (Dt.StrPointerF ("eol", [||])))]

let crapsort2 () = testMachineNoTc
    "    leq(1,1).
    leq(1,2).
    leq(1,3).
    leq(2,2).
    leq(2,3).
    leq(3,3).

    take(c(H,T),H,T).
    take(c(H,T),R,c(H,S)) :- !,take(T,R,S),leq(1,1).


    perm(eol,eol).
    perm(X,c(H,T)) :- take(X,H,R), perm(R,T),leq(1,1).


    sorted(eol).
    sorted(c(H,eol)).
    sorted(c(H1,c(H2,T))) :- leq(H1,H2), sorted(c(H2,T)),leq(1,1).

    terribleSort(X,Y) :- perm(X,Y), sorted(Y),leq(1,1).


    ?- terribleSort(c(2,c(3,c(1,c(2,eol)))),A)"
    [(Dt.HeapPointerF
                (Dt.StrPointerF ("c",
                   [|(Dt.IntF 1);
                     (Dt.StrPointerF ("c",
                        [|(Dt.IntF 2);
                          (Dt.StrPointerF ("c",
                             [|(Dt.IntF 2);
                               (Dt.StrPointerF ("c",
                                  [|(Dt.IntF 3); (Dt.StrPointerF ("eol", [||]))|]
                                  ))
                               |]
                             ))
                          |]
                        ))
                     |]
                   )))
              ]

let tests_notc = [
  "test1" >:: test1;
  "test2" >:: test2;
  "test3" >:: test3;
  "test4" >:: test4;
  "test6" >:: test6;
  "backtrack" >:: backtrack;
  "doubleVar" >:: doubleVar;
  "swaps1" >:: swaps1;
  "noSwaps" >:: noSwaps;
  "swaps2" >:: swaps2;
  "buildStruct1" >:: buildStruct1;
  "buildStruct2" >:: buildStruct2;
  "nestedStructures" >:: nestedStructures;
  "plus1" >:: plus1;
  "plus2" >:: plus2;
  "inStruct" >:: inStruct;
  "alpha" >:: alpha;
  "zeroPLus2" >::  zeroPLus2;
  "fib2" >:: fib2;
  "twop3" >:: twop3;
  "addOneFun" >:: addOneFun;
  "fib4" >:: fib4;
  "fib13" >:: fib13;
  "pairOne" >:: pairOne;
  "cyclicTest" >:: cyclicTest;
  "tripleBacktrack" >:: tripleBacktrack;
  "geqTest" >:: geqTest;
  "quick21" >:: quick21;
  "quick213" >:: quick213;
  "sort21312" >:: sort21312;
  "backtrackDfs" >:: backtrackDfs;
  "backtrackDfs2" >:: backtrackDfs2;
  "testIs" >:: testIs;
  "add2" >:: add2;
  "add3" >:: add3;
  "neg1" >:: neg1;
  "loadArgOrder" >:: loadArgOrder;
  "testBigStruct" >:: testBigStruct;
  "anotherTest" >:: anotherTest;
  "putTest1" >:: putTest1;
  "putTest2" >:: putTest2;
  "build1" >:: build1;
  "aliasing" >:: aliasing;
  "uniffff" >:: uniffff;
  "alia3" >:: alia3;
  "mincut" >:: mincut;
  "headdif1" >:: headdif1;
  "queens8" >:: queens8;
  "bigunif" >:: bigunif;
  "unif1" >:: unif1;
  "unif2" >:: unif2;
  "unif3" >:: unif3;
  "unifStructs" >:: unifStructs;
  "miniperm" >:: miniperm;
  "listy" >:: listy;
  "rightright" >:: rightright;
  "testgetint" >:: testgetint;
  "fibNum" >:: fibNum;
  "clearT" >:: clearT;
  "cutInStack" >:: cutInStack;
  "crapsort1" >:: crapsort1;
  "crapsort2" >:: crapsort2;
]
