open Runall
open Logging
open WriteInstrToFile
open ExecuteFromFile
open Dt

exception EXN

let runtest () =
  let testCase =
    "
    leq(1,1).
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


    ?- terribleSort(c(2,c(3,c(1,c(2,eol)))),A)




"

  in let r = execute testCase false
  in match r with
  | Some(result) ->

    logDebug (fun m -> m "Result: %a" pp_ans result)
  |None ->     logDebug (fun m -> m "no pe ")


let _ = runtest()


let writetest () =
  let testCase =
  "
  iter(0).

  iter(N) :- N1 is N - 1, iter(N1).

  ?- iter(10000000)


"

  in let _ = writeInstructions "instr" testCase
  in let _ = logError (fun m -> m "finish write")
  in ()


let _ = writetest


let runfiletest () =
  let () = runFile "instr"
  in let _ = logError (fun m -> m "finish run")
  in ()


let _ = runfiletest


let type_fail  =
  "

  pred unify(C,C).
  unify(X,X).

  pred f(A,B).
  f(X,Y) :- unify(X,Y).


"

let perf =     "

    h(Z).

    build(null,0).
    build(cons(X),N) :- N1 is N - 1, build(X,N1), h(X).

    run :-  build(X,10000000).

    ?- run
  "

type 'a test = {mutable v : 'a} [@@deriving show]

let later =   "
fib(zero,zero).
fib(s(zero),s(zero)).

fib(X,Y) :- X2 is X - 2, X1 is X - 1, fib(X2,A), fib(X1,B), A is B + Y.

?- fib(s(s(s(s(zero)))),X)
"


let dfsold = "

type list A = cons(A, list(A)), eol.
type tree A = tree(A, tree(A), tree(A)).
type direction = left, right.

pred unify(A,A).
unify(X,X).


pred dfs(A, tree(A), list(direction)).
dfs(V,tree(V,T1,T2),eol).
dfs(V,tree(X,T1,T2),P1) :- dfs(V,T1,P2), unify(P1,cons(left,P2)) .
dfs(V,tree(X,T1,T2),P1) :- dfs(V,T2,P2), unify(P1,cons(right,P2)) .

?- dfs(six,tree(zero,tree(one,tree(five,n,n),tree(six,n,n)),tree(two,tree(three,n,n),tree(four,n,n))),X)
"
