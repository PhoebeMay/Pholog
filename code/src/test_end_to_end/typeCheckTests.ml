
open OUnit
open Dt
open Runall


let testMachineTc input_str expected_result =
  let res = (execute input_str true)
  in match res with
    Some(r) ->
    assert_equal r expected_result
  | None -> raise Oops


let simpleTypesPass () = testMachineTc
    "


type cute = dusk.

pred animal(cute).

animal(dusk).
?- animal(X)
"
    [(Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])))]

(* let testMachineTc input_str expected_result =
   assert_equal (execute input_str true) expected_result *)


let typList () = testMachineTc
    "
type cute = dusk.

type list A = nil, cons(A,list(A)).

pred addOne(list(cute), list(cute)).
addOne(X,cons(dusk,X)).

?- addOne(nil,X)
"
    [(Dt.HeapPointerF
        (Dt.StrPointerF ("cons",
                         [|(Dt.StrPointerF ("dusk", [||]));
                           (Dt.StrPointerF ("nil", [||]))|]
                        )))
    ]


let simpleTypesFail () = try (
  let _ = execute
      "
type cute = dusk.

pred animal(cute).

animal(nottyp).
?- animal(X)
"
      true
  in raise Oops
)
  with (Err(_)) -> ()
     | Not_found -> ()


let tfaillist () = try (
  let _ = execute
      "
  type cute = dusk.

  type list A = nil, cons(A,list(A)).

  pred addOne(list(cute), list(cute)).
  addOne(X,cons(dusk,X)).

  ?- addOne(nugget,X)
"true
  in raise Oops
)   with (Err(_)) -> ()
       | Not_found -> ()


let addnugget () =
  try (
    let _ = execute
        "
    type cute = dusk.

    type list A = nil, cons(A,list(A)).

    type tasty = nugget.

    pred addOne(list(cute), list(cute)).
    addOne(X,cons(dusk,X)).

    ?- addOne(nugget,X)
    "
        true
    in raise Oops
  )   with (Err(_)) -> ()


let tfail1 () =
  try (
    let _ = execute
        "
    type cute = dusk.
    type tasty = nugget.

    type list A = nil, cons(A,list(A)).

    pred addOne(list(tasty), list(cute)).
    addOne(X,cons(dusk,X)).

    ?- addOne(nil,X)
    "
        true
    in raise Oops
  )   with (Err(_)) -> ()


let tfail2 () =
  try (
    let _ = execute
        "
    type cute = dusk.
    type tasty = nugget.

    type list A = nil, cons(A,list(A)).

    pred addOne(list(cute), list(tasty)).
    addOne(X,cons(dusk,X)).

    ?- addOne(nil,X)
    "
        true
    in raise Oops
  )   with (Err(_)) -> ()
         | Not_found -> ()



let tcheckDfs () =
  testMachineTc
    "

      type list A = cons(A, list(A)), eol.
      type tree A = tree(A, tree(A), tree(A)), n.
      type direction = left, right.
      type number = zero,one,two,three,four,five,six.

      pred unify(A,A).
      unify(X,X).


      pred dfs(A, tree(A), list(direction)).
      dfs(V,tree(V,T1,T2),eol).
      dfs(V,tree(X,T1,T2),P1) :- dfs(V,T1,P2), unify(P1,cons(left,P2)) .
      dfs(V,tree(X,T1,T2),P1) :- dfs(V,T2,P2), unify(P1,cons(right,P2)) .

      ?- dfs(six,tree(zero,tree(one,tree(five,n,n),tree(six,n,n)),tree(two,tree(three,n,n),tree(four,n,n))),X)
"
    [(Dt.HeapPointerF
        (Dt.StrPointerF ("cons",
                         [|(Dt.StrPointerF ("left", [||]));
                           (Dt.StrPointerF ("cons",
                                            [|(Dt.StrPointerF ("right", [||]));
                                              (Dt.StrPointerF ("eol", [||]))|]
                                           ))
                         |]
                        )))
    ]



let tfail3 () =
  try (
    let _ = execute
        "

  type cute = dusk.
  type scary = tripos.

  pred unify(A,A).
  unify(X,X).


  ?- unify(dusk,tripos)
  "
        true
    in raise Oops
  )   with (Dt.Err("types no unify")
           ) -> ()


let noGeneralise () =
  try (
    let _ = execute
        "

  type cute = dusk.

  pred unify(A,A).
  unify(dusk,dusk).


  ?- unify(dusk,dusk)
  "
        true
    in raise Oops
  )   with (Dt.Err("Type mismatch expected type (ParseTree.TypeVar \"A\") saw struct dusk")
           ) -> ()





let mustBeGeneral () =
  try (
    let _ = execute
        "
    pred unify(C,C).
    unify(X,X).

    pred f(A,B).
    f(X,Y) :- unify(X,Y).

    "
        true
    in raise Oops
  )   with (Dt.Err(_)
           ) -> ()


let dogfail1 () =
  try (
    let _ = execute
        "type dog = dusk.
    type cat = joker.

    pred unify(A,A).
    unify(X,X).

    pred isCat(cat).
    isCat(joker).

    pred isDog(dog).
    isDog(dusk).


    pred f.
    f :- isCat(X), isDog(Y), unify(X,Y).
    "

        true
    in raise Oops
  )   with (Dt.Err("types no unify")
           ) -> ()


let dogfail2 () =
  try (
    let _ = execute
        "
        type cat = joker.
        pred iscat(cat).
        iscat(joker).

        type dog = dusk.
        pred isdog(dog).
        isdog(dusk).

        pred unify(A,A).
        unify(X,X).

        type arb A = f(A).
        type arb2 A = g(A).

        pred f.
        f :- unify(f(X),f(Y)), iscat(X), isdog(Y).
               "

        true
    in raise Oops
  )   with (Dt.Err("types no unify")
           ) -> ()


let unifystructfail () =
  try (
    let _ = execute
        "
        pred unify(A,A).
        unify(X,X).

        type arb A = f(A).

        type arb2 A = g(A).

        pred f.
        f :- unify(f(X),g(Y)).

        "

        true
    in raise Oops
  )   with (Dt.Err("types no unify")
           ) -> ()



let notdef () =
  try (
    let _ = execute

        "
               pred addOne(wot,wot).
               addOne(X,Y).

               "


        true
    in raise Oops
  )   with Err(_) -> ()



let useint () =
  testMachineTc
    "
pred addOne(int,int).
addOne(X,Y).

"
    []


let isexpr () = testMachineTc
    "
pred addOne(int,int).
addOne(X,Y) :- Y is X + 1.

?- addOne(2,Y)
"
    [(Dt.HeapPointerF (Dt.IntF 3))]


let dfs_distinct () = testMachineTc
    "

  type list A = cons(A, list(A)), eol.
  type tree A = node(A, tree(A), tree(A)), n.
  type direction = left, right.
  type number = zero,one,two,three,four,five,six.

  pred unify(A,A).
  unify(X,X).


  pred dfs(A, tree(A), list(direction)).
  dfs(V,node(V,T1,T2),eol).
  dfs(V,node(X,T1,T2),P1) :- dfs(V,T1,P2), unify(P1,cons(left,P2)) .
  dfs(V,node(X,T1,T2),P1) :- dfs(V,T2,P2), unify(P1,cons(right,P2)) .

  ?- dfs(six,node(zero,node(one,node(five,n,n),node(six,n,n)),node(two,node(three,n,n),node(four,n,n))),X)
"
    [(Dt.HeapPointerF
        (Dt.StrPointerF ("cons",
                         [|(Dt.StrPointerF ("left", [||]));
                           (Dt.StrPointerF ("cons",
                                            [|(Dt.StrPointerF ("right", [||]));
                                              (Dt.StrPointerF ("eol", [||]))|]
                                           ))
                         |]
                        )))
    ]

let isbig () = testMachineTc
    "
type dog = dusk.

pred addOne(int,int).
addOne(X,Y) :- Y is 1 - X + 1.

?- addOne(2,Y)
"
    [(Dt.HeapPointerF (Dt.IntF 0))]

let trickt () =


  try (
    let _ = execute


        "pred unify(A,A).
        unify(X,X).

        pred f(A,B).
        f(X,Y) :- unify(X,Z), unify(Y,Z)."



        true
    in raise Oops
  )   with Err(_) -> ()

let intprob () =
  try (
    let _ = execute "    type cute = dusk.

    pred unify(A).
    unify(1).
"
        true
in raise Oops
  ) with Err(_) -> ()


let pass () = testMachineTc
"    pred unify(A,A).
    unify(X,X).

    pred p(A,B).
    p(X,Y) :- unify(Z,Y)."
[]

let nestrstruct () = testMachineTc
    "          type list A = cons(A, list(A)), eol.
          type pair A, B = mkpair(A,B).
          type map A,B = mkmap(list(pair(A,B))).
          type opt A = some(A), none.

          pred mapLookup(A, map(A,B), opt(B)).
          mapLookup(V, mkmap(eol), none).
          mapLookup(V, mkmap(cons(mkpair(V,K),Tail)), some(K)).
          mapLookup(V, mkmap(cons(H,T)),Result) :- mapLookup(V,mkmap(T),Result).

"
    []


let neststruct2 () = testMachineTc
"  type list A = cons(A, list(A)), eol.
  type pair A, B = mkpair(A,B).
  type opt A = some(A), none.

  pred mapLookup(A, list(pair(A,B)), opt(B)).
  mapLookup(V, eol, none).
  mapLookup(V, cons(mkpair(V,K),Tail), some(K)).
  mapLookup(V, cons(H,T), Result) :- mapLookup(V,T,Result)."
[]

let tests_tc = [
  "simpleTypesPass" >:: simpleTypesPass;
  "simpleTypesFail" >:: simpleTypesFail;
  "typList" >:: typList;
  "tfaillist" >:: tfaillist;
  "addnugget" >:: addnugget;
  "tfail1" >:: tfail1;
  "tfail2" >:: tfail2;
  "tcheckDfs" >:: tcheckDfs;
  "tfail3" >:: tfail3;
  "noGeneralise" >:: noGeneralise;
  "mustBeGeneral" >:: mustBeGeneral;
  "dogfail1" >:: dogfail1;
  "unifystructfail" >:: unifystructfail;
  "dogfail2" >:: dogfail2;
  "notdef" >:: notdef;
  "useint" >:: useint;
  "isexpr" >:: isexpr;
  "dfs_distinct" >:: dfs_distinct;
  "isbig" >:: isbig;
  "trickt" >:: trickt;
  "intprob" >:: intprob;
  "pass" >:: pass;
  "nestrstruct" >:: nestrstruct;
  "neststruct2" >:: neststruct2;
]
