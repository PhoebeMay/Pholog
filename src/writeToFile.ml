open Runall
open Logging
open WriteInstrToFile
open ExecuteFromFile
open Dt
open Core

exception EXN

let queen12 =
  ( "\n\
    \            take(c(H,T),H,T).\n\
    \              take(c(H,T),R,c(H,S)) :- take(T,R,S).\n\n\
    \              perm(eol,eol).\n\
    \              perm(X,c(H,T)) :- take(X,H,R), perm(R,T).\n\n\
    \              equals(X, X).\n\
    \              notequals(X, Y) :- equals(X, Y), !, fail.\n\
    \              notequals(X, Y).\n\n\
    \              checkPair(X,Y,Dist) :-\n\
    \                  X1 is X + Dist,\n\
    \                  X2 is X - Dist,\n\
    \              notequals(Y, X1),\n\
    \                  notequals(Y,X2).\n\n\
    \              checkHead(c(X,eol), Y).\n\n\n\
    \              checkHead(c(Q1,c(Q2,T)), Dist) :-\n\
    \              Dist2 is Dist + 1,\n\
    \              checkPair(Q1,Q2, Dist2),\n\
    \              checkHead(c(Q1,T), Dist2).\n\n\
    \              checkDiags(c(X,eol)).\n\
    \              checkDiags(c(Q1,T)) :- checkHead(c(Q1,T),0), checkDiags(T).\n\n\
    \              queens8(R) :- \
     perm(c(1,c(2,c(3,c(4,c(5,c(6,c(7,c(8,eol)))))))),R), checkDiags(R).\n\n\n\n\
    \      ?-  \
     perm(c(1,c(2,c(3,c(4,c(5,c(6,c(7,c(8,c(9,c(10,c(11,c(12,eol)))))))))))),R), \
     checkDiags(R)",
    "12queen" )

let load_file f = In_channel.read_all f

let queen4 =
  ( "\n\
    \                  take(c(H,T),H,T).\n\
    \                    take(c(H,T),R,c(H,S)) :- take(T,R,S).\n\n\
    \                    perm(eol,eol).\n\
    \                    perm(X,c(H,T)) :- take(X,H,R), perm(R,T).\n\n\
    \                    equals(X, X).\n\
    \                    notequals(X, Y) :- equals(X, Y), !, fail.\n\
    \                    notequals(X, Y).\n\n\
    \                    checkPair(X,Y,Dist) :-\n\
    \                        X1 is X + Dist,\n\
    \                        X2 is X - Dist,\n\
    \                    notequals(Y, X1),\n\
    \                        notequals(Y,X2).\n\n\
    \                    checkHead(c(X,eol), Y).\n\n\n\
    \                    checkHead(c(Q1,c(Q2,T)), Dist) :-\n\
    \                    Dist2 is Dist + 1,\n\
    \                    checkPair(Q1,Q2, Dist2),\n\
    \                    checkHead(c(Q1,T), Dist2).\n\n\
    \                    checkDiags(c(X,eol)).\n\
    \                    checkDiags(c(Q1,T)) :- checkHead(c(Q1,T),0), \
     checkDiags(T).\n\n\n\n\
    \            ?-  perm(c(1,c(2,c(3,c(4,eol)))),R), checkDiags(R)",
    "4queen" )

let write () =
  let name = Sys.argv.(1) in
  let tc_str = Sys.argv.(2) in
  let tc = if tc_str = "1" then true else false in
  let code = load_file ("bench/" ^ name) in
  let location = "bcode/" ^ name in
  let _ = writeInstructions location code tc in
  let _ = logDebug (fun m -> m "finish write") in
  ()

let _ = write ()
