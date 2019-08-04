open Runall
open Logging
open WriteInstrToFile
open ExecuteFromFile
open Dt
open Core

exception EXN

let runtest () =
  let testCase =
    "\n\
    \    leq(1,1).\n\
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
    \    ?- terribleSort(c(2,c(3,c(1,c(2,eol)))),A)\n\n\n\n\n"
  in
  let r = execute testCase false in
  match r with
  | Some result -> logDebug (fun m -> m "Result: %a" pp_ans result)
  | None -> logDebug (fun m -> m "no pe ")

let _ = runtest ()

let writetest () =
  let testCase =
    "\n\
    \  iter(0).\n\n\
    \  iter(N) :- N1 is N - 1, iter(N1).\n\n\
    \  ?- iter(10000000)\n\n\n"
  in
  let _ = writeInstructions "instr" testCase in
  let _ = logError (fun m -> m "finish write") in
  ()

let _ = writetest

let runfiletest () =
  let () = runFile "instr" in
  let _ = logError (fun m -> m "finish run") in
  ()

let _ = runfiletest

let type_fail =
  "\n\n\
  \  pred unify(C,C).\n\
  \  unify(X,X).\n\n\
  \  pred f(A,B).\n\
  \  f(X,Y) :- unify(X,Y).\n\n\n"

let perf =
  "\n\n\
  \    h(Z).\n\n\
  \    build(null,0).\n\
  \    build(cons(X),N) :- N1 is N - 1, build(X,N1), h(X).\n\n\
  \    run :-  build(X,10000000).\n\n\
  \    ?- run\n\
  \  "

type 'a test = { mutable v : 'a } [@@deriving show]

let later =
  "\n\
   fib(zero,zero).\n\
   fib(s(zero),s(zero)).\n\n\
   fib(X,Y) :- X2 is X - 2, X1 is X - 1, fib(X2,A), fib(X1,B), A is B + Y.\n\n\
   ?- fib(s(s(s(s(zero)))),X)\n"

let dfsold =
  "\n\n\
   type list A = cons(A, list(A)), eol.\n\
   type tree A = tree(A, tree(A), tree(A)).\n\
   type direction = left, right.\n\n\
   pred unify(A,A).\n\
   unify(X,X).\n\n\n\
   pred dfs(A, tree(A), list(direction)).\n\
   dfs(V,tree(V,T1,T2),eol).\n\
   dfs(V,tree(X,T1,T2),P1) :- dfs(V,T1,P2), unify(P1,cons(left,P2)) .\n\
   dfs(V,tree(X,T1,T2),P1) :- dfs(V,T2,P2), unify(P1,cons(right,P2)) .\n\n\
   ?- \
   dfs(six,tree(zero,tree(one,tree(five,n,n),tree(six,n,n)),tree(two,tree(three,n,n),tree(four,n,n))),X)\n"
