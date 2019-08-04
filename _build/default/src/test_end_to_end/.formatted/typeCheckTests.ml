open OUnit
open Dt
open Runall

let testMachineTc input_str expected_result =
  let res = execute input_str true in
  match res with
  | Some r -> assert_equal r expected_result
  | None -> raise Oops

let simpleTypesPass () =
  testMachineTc
    "\n\n\n\
     type cute = dusk.\n\n\
     pred animal(cute).\n\n\
     animal(dusk).\n\
     ?- animal(X)\n"
    [ Dt.HeapPointerF (Dt.StrPointerF ("dusk", [||])) ]

(* let testMachineTc input_str expected_result =
   assert_equal (execute input_str true) expected_result *)

let typList () =
  testMachineTc
    "\n\
     type cute = dusk.\n\n\
     type list A = nil, cons(A,list(A)).\n\n\
     pred addOne(list(cute), list(cute)).\n\
     addOne(X,cons(dusk,X)).\n\n\
     ?- addOne(nil,X)\n"
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "cons",
             [| Dt.StrPointerF ("dusk", [||]); Dt.StrPointerF ("nil", [||]) |]
           ))
    ]

let simpleTypesFail () =
  try
    let _ =
      execute
        "\n\
         type cute = dusk.\n\n\
         pred animal(cute).\n\n\
         animal(nottyp).\n\
         ?- animal(X)\n"
        true
    in
    raise Oops
  with
  | Err _ -> ()
  | Not_found -> ()

let tfaillist () =
  try
    let _ =
      execute
        "\n\
        \  type cute = dusk.\n\n\
        \  type list A = nil, cons(A,list(A)).\n\n\
        \  pred addOne(list(cute), list(cute)).\n\
        \  addOne(X,cons(dusk,X)).\n\n\
        \  ?- addOne(nugget,X)\n"
        true
    in
    raise Oops
  with
  | Err _ -> ()
  | Not_found -> ()

let addnugget () =
  try
    let _ =
      execute
        "\n\
        \    type cute = dusk.\n\n\
        \    type list A = nil, cons(A,list(A)).\n\n\
        \    type tasty = nugget.\n\n\
        \    pred addOne(list(cute), list(cute)).\n\
        \    addOne(X,cons(dusk,X)).\n\n\
        \    ?- addOne(nugget,X)\n\
        \    "
        true
    in
    raise Oops
  with Err _ -> ()

let tfail1 () =
  try
    let _ =
      execute
        "\n\
        \    type cute = dusk.\n\
        \    type tasty = nugget.\n\n\
        \    type list A = nil, cons(A,list(A)).\n\n\
        \    pred addOne(list(tasty), list(cute)).\n\
        \    addOne(X,cons(dusk,X)).\n\n\
        \    ?- addOne(nil,X)\n\
        \    "
        true
    in
    raise Oops
  with Err _ -> ()

let tfail2 () =
  try
    let _ =
      execute
        "\n\
        \    type cute = dusk.\n\
        \    type tasty = nugget.\n\n\
        \    type list A = nil, cons(A,list(A)).\n\n\
        \    pred addOne(list(cute), list(tasty)).\n\
        \    addOne(X,cons(dusk,X)).\n\n\
        \    ?- addOne(nil,X)\n\
        \    "
        true
    in
    raise Oops
  with
  | Err _ -> ()
  | Not_found -> ()

let tcheckDfs () =
  testMachineTc
    "\n\n\
    \      type list A = cons(A, list(A)), eol.\n\
    \      type tree A = tree(A, tree(A), tree(A)), n.\n\
    \      type direction = left, right.\n\
    \      type number = zero,one,two,three,four,five,six.\n\n\
    \      pred unify(A,A).\n\
    \      unify(X,X).\n\n\n\
    \      pred dfs(A, tree(A), list(direction)).\n\
    \      dfs(V,tree(V,T1,T2),eol).\n\
    \      dfs(V,tree(X,T1,T2),P1) :- dfs(V,T1,P2), unify(P1,cons(left,P2)) .\n\
    \      dfs(V,tree(X,T1,T2),P1) :- dfs(V,T2,P2), unify(P1,cons(right,P2)) .\n\n\
    \      ?- \
     dfs(six,tree(zero,tree(one,tree(five,n,n),tree(six,n,n)),tree(two,tree(three,n,n),tree(four,n,n))),X)\n"
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "cons",
             [| Dt.StrPointerF ("left", [||]);
                Dt.StrPointerF
                  ( "cons",
                    [| Dt.StrPointerF ("right", [||]);
                       Dt.StrPointerF ("eol", [||])
                    |] )
             |] ))
    ]

let tfail3 () =
  try
    let _ =
      execute
        "\n\n\
        \  type cute = dusk.\n\
        \  type scary = tripos.\n\n\
        \  pred unify(A,A).\n\
        \  unify(X,X).\n\n\n\
        \  ?- unify(dusk,tripos)\n\
        \  "
        true
    in
    raise Oops
  with Dt.Err "types no unify" -> ()

let noGeneralise () =
  try
    let _ =
      execute
        "\n\n\
        \  type cute = dusk.\n\n\
        \  pred unify(A,A).\n\
        \  unify(dusk,dusk).\n\n\n\
        \  ?- unify(dusk,dusk)\n\
        \  "
        true
    in
    raise Oops
  with
  | Dt.Err
      "Type mismatch expected type (ParseTree.TypeVar \"A\") saw struct dusk"
  ->
    ()

let mustBeGeneral () =
  try
    let _ =
      execute
        "\n\
        \    pred unify(C,C).\n\
        \    unify(X,X).\n\n\
        \    pred f(A,B).\n\
        \    f(X,Y) :- unify(X,Y).\n\n\
        \    "
        true
    in
    raise Oops
  with Dt.Err _ -> ()

let dogfail1 () =
  try
    let _ =
      execute
        "type dog = dusk.\n\
        \    type cat = joker.\n\n\
        \    pred unify(A,A).\n\
        \    unify(X,X).\n\n\
        \    pred isCat(cat).\n\
        \    isCat(joker).\n\n\
        \    pred isDog(dog).\n\
        \    isDog(dusk).\n\n\n\
        \    pred f.\n\
        \    f :- isCat(X), isDog(Y), unify(X,Y).\n\
        \    "
        true
    in
    raise Oops
  with Dt.Err "types no unify" -> ()

let dogfail2 () =
  try
    let _ =
      execute
        "\n\
        \        type cat = joker.\n\
        \        pred iscat(cat).\n\
        \        iscat(joker).\n\n\
        \        type dog = dusk.\n\
        \        pred isdog(dog).\n\
        \        isdog(dusk).\n\n\
        \        pred unify(A,A).\n\
        \        unify(X,X).\n\n\
        \        type arb A = f(A).\n\
        \        type arb2 A = g(A).\n\n\
        \        pred f.\n\
        \        f :- unify(f(X),f(Y)), iscat(X), isdog(Y).\n\
        \               "
        true
    in
    raise Oops
  with Dt.Err "types no unify" -> ()

let unifystructfail () =
  try
    let _ =
      execute
        "\n\
        \        pred unify(A,A).\n\
        \        unify(X,X).\n\n\
        \        type arb A = f(A).\n\n\
        \        type arb2 A = g(A).\n\n\
        \        pred f.\n\
        \        f :- unify(f(X),g(Y)).\n\n\
        \        "
        true
    in
    raise Oops
  with Dt.Err "types no unify" -> ()

let notdef () =
  try
    let _ =
      execute
        "\n\
        \               pred addOne(wot,wot).\n\
        \               addOne(X,Y).\n\n\
        \               "
        true
    in
    raise Oops
  with Err _ -> ()

let useint () = testMachineTc "\npred addOne(int,int).\naddOne(X,Y).\n\n" []

let isexpr () =
  testMachineTc
    "\npred addOne(int,int).\naddOne(X,Y) :- Y is X + 1.\n\n?- addOne(2,Y)\n"
    [ Dt.HeapPointerF (Dt.IntF 3) ]

let dfs_distinct () =
  testMachineTc
    "\n\n\
    \  type list A = cons(A, list(A)), eol.\n\
    \  type tree A = node(A, tree(A), tree(A)), n.\n\
    \  type direction = left, right.\n\
    \  type number = zero,one,two,three,four,five,six.\n\n\
    \  pred unify(A,A).\n\
    \  unify(X,X).\n\n\n\
    \  pred dfs(A, tree(A), list(direction)).\n\
    \  dfs(V,node(V,T1,T2),eol).\n\
    \  dfs(V,node(X,T1,T2),P1) :- dfs(V,T1,P2), unify(P1,cons(left,P2)) .\n\
    \  dfs(V,node(X,T1,T2),P1) :- dfs(V,T2,P2), unify(P1,cons(right,P2)) .\n\n\
    \  ?- \
     dfs(six,node(zero,node(one,node(five,n,n),node(six,n,n)),node(two,node(three,n,n),node(four,n,n))),X)\n"
    [ Dt.HeapPointerF
        (Dt.StrPointerF
           ( "cons",
             [| Dt.StrPointerF ("left", [||]);
                Dt.StrPointerF
                  ( "cons",
                    [| Dt.StrPointerF ("right", [||]);
                       Dt.StrPointerF ("eol", [||])
                    |] )
             |] ))
    ]

let isbig () =
  testMachineTc
    "\n\
     type dog = dusk.\n\n\
     pred addOne(int,int).\n\
     addOne(X,Y) :- Y is 1 - X + 1.\n\n\
     ?- addOne(2,Y)\n"
    [ Dt.HeapPointerF (Dt.IntF 0) ]

let trickt () =
  try
    let _ =
      execute
        "pred unify(A,A).\n\
        \        unify(X,X).\n\n\
        \        pred f(A,B).\n\
        \        f(X,Y) :- unify(X,Z), unify(Y,Z)."
        true
    in
    raise Oops
  with Err _ -> ()

let intprob () =
  try
    let _ =
      execute "    type cute = dusk.\n\n    pred unify(A).\n    unify(1).\n"
        true
    in
    raise Oops
  with Err _ -> ()

let pass () =
  testMachineTc
    "    pred unify(A,A).\n\
    \    unify(X,X).\n\n\
    \    pred p(A,B).\n\
    \    p(X,Y) :- unify(Z,Y)."
    []

let nestrstruct () =
  testMachineTc
    "          type list A = cons(A, list(A)), eol.\n\
    \          type pair A, B = mkpair(A,B).\n\
    \          type map A,B = mkmap(list(pair(A,B))).\n\
    \          type opt A = some(A), none.\n\n\
    \          pred mapLookup(A, map(A,B), opt(B)).\n\
    \          mapLookup(V, mkmap(eol), none).\n\
    \          mapLookup(V, mkmap(cons(mkpair(V,K),Tail)), some(K)).\n\
    \          mapLookup(V, mkmap(cons(H,T)),Result) :- \
     mapLookup(V,mkmap(T),Result).\n\n"
    []

let neststruct2 () =
  testMachineTc
    "  type list A = cons(A, list(A)), eol.\n\
    \  type pair A, B = mkpair(A,B).\n\
    \  type opt A = some(A), none.\n\n\
    \  pred mapLookup(A, list(pair(A,B)), opt(B)).\n\
    \  mapLookup(V, eol, none).\n\
    \  mapLookup(V, cons(mkpair(V,K),Tail), some(K)).\n\
    \  mapLookup(V, cons(H,T), Result) :- mapLookup(V,T,Result)."
    []

let tests_tc =
  [ "simpleTypesPass" >:: simpleTypesPass;
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
    "neststruct2" >:: neststruct2
  ]
