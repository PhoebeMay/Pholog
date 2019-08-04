open Interp
open ParseTree

let test_case =
"
notequals(n1,n2).
notequals(n2,n1).
notequals(n2,n3).
notequals(n3,n1).
notequals(n3,n2).
notequals(n1,n3).

add(n1,n1,n2).
add(n2,n1,n3).

take(c(H,T),H,T).
take(c(H,T),R,c(H,S)) :- take(T,R,S).

perm(eol,eol).
perm(X,c(H,T)) :- take(X,H,R), perm(R,T).


checkHeadDiffNot1(c(Q1,c(Q2,T))) :-
add(Q1,n1,Dist2),
notequals(Q2, Dist2).

?- perm(c(n1,c(n2,eol)),R), checkHeadDiffNot1(R)
"

let () =
  Printf.printf "Interp 4\n";
  print_endline "\n";
  let lexbuf = Lexing.from_string test_case in
  let parseTree = Parser.main Lexer.token lexbuf in
  let result = interpret parseTree in
     Printf.printf "The answer is:\n%s\n" (strAns result)


let _ = "
    add(n0,n0,n0).
    add(n0,n1,n1).
    add(n0,n2,n2).
    add(n0,n3,n3).
    add(n0,n4,n4).
    add(n0,n5,n5).
    add(n0,n6,n6).
    add(n0,n7,n7).
    add(n0,n8,n8).
    add(n0,n9,n9).
    add(n1,n0,n1).
    add(n1,n1,n2).
    add(n1,n2,n3).
    add(n1,n3,n4).
    add(n1,n4,n5).
    add(n1,n5,n6).
    add(n1,n6,n7).
    add(n1,n7,n8).
    add(n1,n8,n9).
    add(n1,n9,n10).
    add(n2,n0,n2).
    add(n2,n1,n3).
    add(n2,n2,n4).
    add(n2,n3,n5).
    add(n2,n4,n6).
    add(n2,n5,n7).
    add(n2,n6,n8).
    add(n2,n7,n9).
    add(n2,n8,n10).
    add(n2,n9,n11).
    add(n3,n0,n3).
    add(n3,n1,n4).
    add(n3,n2,n5).
    add(n3,n3,n6).
    add(n3,n4,n7).
    add(n3,n5,n8).
    add(n3,n6,n9).
    add(n3,n7,n10).
    add(n3,n8,n11).
    add(n3,n9,n12).
    add(n4,n0,n4).
    add(n4,n1,n5).
    add(n4,n2,n6).
    add(n4,n3,n7).
    add(n4,n4,n8).
    add(n4,n5,n9).
    add(n4,n6,n10).
    add(n4,n7,n11).
    add(n4,n8,n12).
    add(n4,n9,n13).
    add(n5,n0,n5).
    add(n5,n1,n6).
    add(n5,n2,n7).
    add(n5,n3,n8).
    add(n5,n4,n9).
    add(n5,n5,n10).
    add(n5,n6,n11).
    add(n5,n7,n12).
    add(n5,n8,n13).
    add(n5,n9,n14).
    add(n6,n0,n6).
    add(n6,n1,n7).
    add(n6,n2,n8).
    add(n6,n3,n9).
    add(n6,n4,n10).
    add(n6,n5,n11).
    add(n6,n6,n12).
    add(n6,n7,n13).
    add(n6,n8,n14).
    add(n6,n9,n15).
    add(n7,n0,n7).
    add(n7,n1,n8).
    add(n7,n2,n9).
    add(n7,n3,n10).
    add(n7,n4,n11).
    add(n7,n5,n12).
    add(n7,n6,n13).
    add(n7,n7,n14).
    add(n7,n8,n15).
    add(n7,n9,n16).
    add(n8,n0,n8).
    add(n8,n1,n9).
    add(n8,n2,n10).
    add(n8,n3,n11).
    add(n8,n4,n12).
    add(n8,n5,n13).
    add(n8,n6,n14).
    add(n8,n7,n15).
    add(n8,n8,n16).
    add(n8,n9,n17).
    add(n9,n0,n9).
    add(n9,n1,n10).
    add(n9,n2,n11).
    add(n9,n3,n12).
    add(n9,n4,n13).
    add(n9,n5,n14).
    add(n9,n6,n15).
    add(n9,n7,n16).
    add(n9,n8,n17).
    add(n9,n9,n18).


    sub(n0,n0,n0).
    sub(n0,n1,nm1).
    sub(n0,n2,nm2).
    sub(n0,n3,nm3).
    sub(n0,n4,nm4).
    sub(n0,n5,nm5).
    sub(n0,n6,nm6).
    sub(n0,n7,nm7).
    sub(n0,n8,nm8).
    sub(n0,n9,nm9).
    sub(n1,n0,n1).
    sub(n1,n1,n0).
    sub(n1,n2,nm1).
    sub(n1,n3,nm2).
    sub(n1,n4,nm3).
    sub(n1,n5,nm4).
    sub(n1,n6,nm5).
    sub(n1,n7,nm6).
    sub(n1,n8,nm7).
    sub(n1,n9,nm8).
    sub(n2,n0,n2).
    sub(n2,n1,n1).
    sub(n2,n2,n0).
    sub(n2,n3,nm1).
    sub(n2,n4,nm2).
    sub(n2,n5,nm3).
    sub(n2,n6,nm4).
    sub(n2,n7,nm5).
    sub(n2,n8,nm6).
    sub(n2,n9,nm7).
    sub(n3,n0,n3).
    sub(n3,n1,n2).
    sub(n3,n2,n1).
    sub(n3,n3,n0).
    sub(n3,n4,nm1).
    sub(n3,n5,nm2).
    sub(n3,n6,nm3).
    sub(n3,n7,nm4).
    sub(n3,n8,nm5).
    sub(n3,n9,nm6).
    sub(n4,n0,n4).
    sub(n4,n1,n3).
    sub(n4,n2,n2).
    sub(n4,n3,n1).
    sub(n4,n4,n0).
    sub(n4,n5,nm1).
    sub(n4,n6,nm2).
    sub(n4,n7,nm3).
    sub(n4,n8,nm4).
    sub(n4,n9,nm5).
    sub(n5,n0,n5).
    sub(n5,n1,n4).
    sub(n5,n2,n3).
    sub(n5,n3,n2).
    sub(n5,n4,n1).
    sub(n5,n5,n0).
    sub(n5,n6,nm1).
    sub(n5,n7,nm2).
    sub(n5,n8,nm3).
    sub(n5,n9,nm4).
    sub(n6,n0,n6).
    sub(n6,n1,n5).
    sub(n6,n2,n4).
    sub(n6,n3,n3).
    sub(n6,n4,n2).
    sub(n6,n5,n1).
    sub(n6,n6,n0).
    sub(n6,n7,nm1).
    sub(n6,n8,nm2).
    sub(n6,n9,nm3).
    sub(n7,n0,n7).
    sub(n7,n1,n6).
    sub(n7,n2,n5).
    sub(n7,n3,n4).
    sub(n7,n4,n3).
    sub(n7,n5,n2).
    sub(n7,n6,n1).
    sub(n7,n7,n0).
    sub(n7,n8,nm1).
    sub(n7,n9,nm2).
    sub(n8,n0,n8).
    sub(n8,n1,n7).
    sub(n8,n2,n6).
    sub(n8,n3,n5).
    sub(n8,n4,n4).
    sub(n8,n5,n3).
    sub(n8,n6,n2).
    sub(n8,n7,n1).
    sub(n8,n8,n0).
    sub(n8,n9,nm1).
    sub(n9,n0,n9).
    sub(n9,n1,n8).
    sub(n9,n2,n7).
    sub(n9,n3,n6).
    sub(n9,n4,n5).
    sub(n9,n5,n4).
    sub(n9,n6,n3).
    sub(n9,n7,n2).
    sub(n9,n8,n1).
    sub(n9,n9,n0).


    take(c(H,T),H,T).
    take(c(H,T),R,c(H,S)) :- take(T,R,S).

    perm(eol,eol).
    perm(X,c(H,T)) :- take(X,H,R), perm(R,T).

    equals(X, X).
    notequals(X, Y) :- equals(X, Y), !, fail.
    notequals(X, Y).

    checkPair(X,Y,Dist) :-
    add(X,Dist,X1),
    sub(X,Dist,X2),
    notequals(Y, X1),
    notequals(Y,X2).

    checkHead(c(X,eol), Y).


    checkHead(c(Q1,c(Q2,T)), Dist) :-
    add(Dist,n1,Dist2),
    checkPair(Q1,Q2, Dist2),
    checkHead(c(Q1,T), Dist2).

    checkDiags(c(X,eol)).
    checkDiags(c(Q1,T)) :- checkHead(c(Q1,T),n0), checkDiags(T).

?- perm(c(n1,c(n2,c(n3,c(n4,eol)))),R), checkDiags(R)"
