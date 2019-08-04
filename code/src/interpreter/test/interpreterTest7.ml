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


let t9 () = testInterpreter
    "
   person(craig).
   dog(dusk).
   ?- person(X), dog(Y)
   "
    (Ans(Resolvant([Atom("person",[TFun("craig",[])]);Atom("dog",[TFun("dusk",[])])])))

let no_swaps () = testInterpreter
    "
g(dusk).
h(craig).

f(A,B) :- g(A), h(B).

?-f(Y,X)
"
    (Ans(Resolvant([Atom("f",[TFun("dusk",[]);TFun("craig",[])])]))
    )

let swaps1 () = testInterpreter   "
g(dusk).
h(craig).

f(X,Y) :- g(X), h(Y).

?-f(Y,X)
  "
    (Ans(Resolvant([Atom("f",[TFun("dusk",[]);TFun("craig",[])])])))


let swaps2 () = testInterpreter
    "
g(dusk).
h(craig).

f(X,Y) :- g(Y), h(X).

?-f(Y,X)
"
    (Ans(Resolvant([Atom("f",[TFun("craig",[]);TFun("dusk",[])])]))
    )

let structarg () = testInterpreter
    "
plant(flower(X)).

?- plant(X)
"
    (Ans(Resolvant([Atom("plant",[TFun("flower",[TVar("(Interp.Argument 0)")])])])))


let addOne () = testInterpreter
    "
add(X,zero,X).
add(X,s(Y),s(Z)) :- add(X,Y,Z).



?- add(zero,s(zero),X)

"
    (Ans(Resolvant([
         Atom("add",[
             TFun("zero",[]);
             TFun("s",[TFun("zero",[])]);
             TFun("s",[TFun("zero",[])])
           ])]))
    )


let onePlusOne () = testInterpreter
    "
add(X,zero,X).
add(X,s(Y),s(Z)) :- add(X,Y,Z).


?- add(s(zero),s(zero),X)

"
    (Ans(Resolvant([
         Atom("add",[
             TFun("s",[TFun("zero",[])]);
             TFun("s",[TFun("zero",[])]);
             TFun("s",[TFun("s",[TFun("zero",[])])])])]))
    )

let alpha1 () = testInterpreter
    "
f(X).

?- f(struct(X))
"
    (Ans(Resolvant([Atom("f",[TFun("struct",[TVar("(Interp.Inner 0)")])])])))



let alpha2 () = testInterpreter
    "
f(struct(X)).

?- f(X)
"
    (Ans(Resolvant([Atom("f",[TFun("struct",[TVar("(Interp.Argument 0)")])])])))

let alpha3 () = testInterpreter
    "
g(build(X)).
f(struct(X)) :- g(X).

?- f(X)
"
    (Ans(Resolvant([
         Atom("f",[TFun("struct",[TFun("build",[TVar("(Interp.Argument 0)")])])]
             )])))


let zeroTwo () = testInterpreter
    "
add(X,zero,X).
add(X,s(Y),s(Z)) :- add(X,Y,Z).


?- add(zero,s(s(zero)),X)

"
    (Ans(Resolvant([
         Atom("add",[TFun("zero",[]);
                     TFun("s",[TFun("s",[TFun("zero",[])])]);
                     TFun("s",[TFun("s",[TFun("zero",[])])])])])))


let fib2 () = testInterpreter
    "


add(X,zero,X).
add(X,s(Y),s(Z)) :- add(X,Y,Z).


fib(zero,zero).
fib(s(zero),s(zero)).

fib(s(s(X)),Y) :- fib(X,A), fib(s(X),B), add(A,B,Y).

?- fib(s(s(zero)),X)

"
    (Ans(Resolvant([Atom("fib",[
         TFun("s",[TFun("s",[TFun("zero",[])])]);
         TFun("s",[TFun("zero",[])])])])))

let twoThree () = testInterpreter
    "
add(X,zero,X).
add(X,s(Y),s(Z)) :- add(X,Y,Z).


?- add(s(s(zero)),s(s(s(zero))),s(X))

"
    (Ans(Resolvant([
         Atom("add",[TFun("s",[TFun("s",[TFun("zero",[])])]);
                     TFun("s",[TFun("s",[TFun("s",[TFun("zero",[])])])]);
                     TFun(
                       "s",[TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("zero",[])])])])])]
                     )])])))


let addOneP () = testInterpreter
    "
add(X,zero,X).
add(X,s(Y),s(Z)) :- add(X,Y,Z).

addOne(X,Y) :- add(X,s(zero),A), add(A,zero,Y).

?- addOne(zero,A)
"
    (Ans(Resolvant([
         Atom("addOne",[
             TFun("zero",[]);
             TFun("s",[TFun("zero",[])])])]))
    )

let fib4 () = testInterpreter
    "
add(X,zero,X).
add(X,s(Y),s(Z)) :- add(X,Y,Z).


fib(zero,zero).
fib(s(zero),s(zero)).

fib(s(s(X)),Y) :- fib(X,A), fib(s(X),B), add(A,B,Y).

?- fib(s(s(s(s(zero)))),X)
"
    (Ans(Resolvant([
         Atom("fib",[
             TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("zero",[])])])])]);
             TFun("s",[TFun("s",[TFun("s",[TFun("zero",[])])])])])]))
    )


let fib13 () = testInterpreter
    "
add(X,zero,X).
add(X,s(Y),s(Z)) :- add(X,Y,Z).


fib(zero,zero).
fib(s(zero),s(zero)).

fib(s(s(X)),Y) :- fib(X,A), fib(s(X),B), add(A,B,Y).

?- fib(s(s(s(s(s(s(s(zero))))))),X)
"
    (Ans(Resolvant([
         Atom("fib",[

             TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("zero",[])])])])])])])]);

             TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("s",[TFun("zero",[])])])])])])])])])])])])])])
           ])])))


let pairOne () = testInterpreter
    "
ll(one,two).

unif(X,X).

less(null,Z, null).
less(pair(H,T), Bound, Result) :- ll(H,Bound), less(T, Bound, NewRes), unif(Result, pair(H, NewRes)).

?- less(pair(one,null),two,Result)
"
    (Ans(Resolvant([
         Atom("less",[
             TFun("pair",
                  [TFun("one",[]);TFun("null",[])]);
             TFun("two",[]);
             TFun("pair",[TFun("one",[]);TFun("null",[])])])]))
    )

let quick21 () =
  testInterpreter

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
    (Ans(Resolvant([
         Atom("quick",[
             TFun("pair",[TFun("two",[]);
                          TFun("pair",[TFun("one",[]);
                                       TFun("null",[])])]);
             TFun("pair",[TFun("one",[]);
                          TFun("pair",[TFun("two",[]);
                                       TFun("null",[])])])])])))




let quick213 () = testInterpreter
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
    (Ans(Resolvant([
         Atom("quick",[
             TFun("pair",[TFun("two",[]);
                          TFun("pair",[TFun("one",[]);
                                       TFun("pair",[TFun("three",[]);
                                                    TFun("null",[])])])]);

             TFun("pair",[TFun("one",[]);
                          TFun("pair",[TFun("two",[]);
                                       TFun("three",[])])])])])))


let sort21312 () =
  testInterpreter
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
    (Ans(Resolvant([
         Atom("quick",[
             TFun("pair",[TFun("two",[]);
                          TFun("pair",[TFun("one",[]);
                                       TFun("pair",[TFun("three",[]);
                                                    TFun("pair",[TFun("one",[]);
                                                                 TFun("pair",[TFun("two",[]);TFun("null",[])])])])])]);

             TFun("pair",[TFun("one",[]);
                          TFun("pair",[TFun("one",[]);
                                       TFun("pair",[TFun("two",[]);
                                                    TFun("pair",[TFun("two",[]);
                                                                 TFun("pair",[TFun("three",[]);TFun("null",[])
                                                                             ])])])])]
                 )])])))

let suite = "OUnit Lexer + Parser Tests" >::: [
    "basic_true" >:: basic_true;
    "basic_false" >:: basic_false;
    "unification" >:: unification;
    "basic_backtrack" >:: basic_backtrack;
    "repeat_var" >:: repeat_var;
    "two_var" >:: two_var;
    "false_1" >:: false_1;
    "priority_order" >:: priority_order;
    "t9" >:: t9;
    "no_swaps" >:: no_swaps;
    "swaps1" >:: swaps1;
    "swaps2" >:: swaps2;
    "structarg" >:: structarg;
    "addOne" >:: addOne;
    "onePlusOne" >:: onePlusOne;
    "alpha1" >:: alpha1;
    "alpha2" >:: alpha2;
    "alpha3" >:: alpha3;
    "zeroTwo" >:: zeroTwo;
    "addOneP" >:: addOneP;
    "fib4" >:: fib4;
    "fib13" >:: fib13;
    "pairOne" >:: pairOne;
    "quick21" >:: quick21;
    "quick213" >:: quick213;
    "sort21312" >:: sort21312;
  ]



let _ =
  run_test_tt_main suite
