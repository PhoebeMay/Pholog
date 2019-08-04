open Interp
open ParseTree

let test_case =
  "\n\
   notequals(n1,n2).\n\
   notequals(n2,n1).\n\
   notequals(n2,n3).\n\
   notequals(n3,n1).\n\
   notequals(n3,n2).\n\
   notequals(n1,n3).\n\n\
   add(n1,n1,n2).\n\
   add(n2,n1,n3).\n\n\
   take(c(H,T),H,T).\n\
   take(c(H,T),R,c(H,S)) :- take(T,R,S).\n\n\
   perm(eol,eol).\n\
   perm(X,c(H,T)) :- take(X,H,R), perm(R,T).\n\n\n\
   checkHeadDiffNot1(c(Q1,c(Q2,T))) :-\n\
   add(Q1,n1,Dist2),\n\
   notequals(Q2, Dist2).\n\n\
   ?- perm(c(n1,c(n2,eol)),R), checkHeadDiffNot1(R)\n"

let () =
  Printf.printf "Interp 4\n";
  print_endline "\n";
  let lexbuf = Lexing.from_string test_case in
  let parseTree = Parser.main Lexer.token lexbuf in
  let result = interpret parseTree in
  Printf.printf "The answer is:\n%s\n" (strAns result)

let _ =
  "\n\
  \    add(n0,n0,n0).\n\
  \    add(n0,n1,n1).\n\
  \    add(n0,n2,n2).\n\
  \    add(n0,n3,n3).\n\
  \    add(n0,n4,n4).\n\
  \    add(n0,n5,n5).\n\
  \    add(n0,n6,n6).\n\
  \    add(n0,n7,n7).\n\
  \    add(n0,n8,n8).\n\
  \    add(n0,n9,n9).\n\
  \    add(n1,n0,n1).\n\
  \    add(n1,n1,n2).\n\
  \    add(n1,n2,n3).\n\
  \    add(n1,n3,n4).\n\
  \    add(n1,n4,n5).\n\
  \    add(n1,n5,n6).\n\
  \    add(n1,n6,n7).\n\
  \    add(n1,n7,n8).\n\
  \    add(n1,n8,n9).\n\
  \    add(n1,n9,n10).\n\
  \    add(n2,n0,n2).\n\
  \    add(n2,n1,n3).\n\
  \    add(n2,n2,n4).\n\
  \    add(n2,n3,n5).\n\
  \    add(n2,n4,n6).\n\
  \    add(n2,n5,n7).\n\
  \    add(n2,n6,n8).\n\
  \    add(n2,n7,n9).\n\
  \    add(n2,n8,n10).\n\
  \    add(n2,n9,n11).\n\
  \    add(n3,n0,n3).\n\
  \    add(n3,n1,n4).\n\
  \    add(n3,n2,n5).\n\
  \    add(n3,n3,n6).\n\
  \    add(n3,n4,n7).\n\
  \    add(n3,n5,n8).\n\
  \    add(n3,n6,n9).\n\
  \    add(n3,n7,n10).\n\
  \    add(n3,n8,n11).\n\
  \    add(n3,n9,n12).\n\
  \    add(n4,n0,n4).\n\
  \    add(n4,n1,n5).\n\
  \    add(n4,n2,n6).\n\
  \    add(n4,n3,n7).\n\
  \    add(n4,n4,n8).\n\
  \    add(n4,n5,n9).\n\
  \    add(n4,n6,n10).\n\
  \    add(n4,n7,n11).\n\
  \    add(n4,n8,n12).\n\
  \    add(n4,n9,n13).\n\
  \    add(n5,n0,n5).\n\
  \    add(n5,n1,n6).\n\
  \    add(n5,n2,n7).\n\
  \    add(n5,n3,n8).\n\
  \    add(n5,n4,n9).\n\
  \    add(n5,n5,n10).\n\
  \    add(n5,n6,n11).\n\
  \    add(n5,n7,n12).\n\
  \    add(n5,n8,n13).\n\
  \    add(n5,n9,n14).\n\
  \    add(n6,n0,n6).\n\
  \    add(n6,n1,n7).\n\
  \    add(n6,n2,n8).\n\
  \    add(n6,n3,n9).\n\
  \    add(n6,n4,n10).\n\
  \    add(n6,n5,n11).\n\
  \    add(n6,n6,n12).\n\
  \    add(n6,n7,n13).\n\
  \    add(n6,n8,n14).\n\
  \    add(n6,n9,n15).\n\
  \    add(n7,n0,n7).\n\
  \    add(n7,n1,n8).\n\
  \    add(n7,n2,n9).\n\
  \    add(n7,n3,n10).\n\
  \    add(n7,n4,n11).\n\
  \    add(n7,n5,n12).\n\
  \    add(n7,n6,n13).\n\
  \    add(n7,n7,n14).\n\
  \    add(n7,n8,n15).\n\
  \    add(n7,n9,n16).\n\
  \    add(n8,n0,n8).\n\
  \    add(n8,n1,n9).\n\
  \    add(n8,n2,n10).\n\
  \    add(n8,n3,n11).\n\
  \    add(n8,n4,n12).\n\
  \    add(n8,n5,n13).\n\
  \    add(n8,n6,n14).\n\
  \    add(n8,n7,n15).\n\
  \    add(n8,n8,n16).\n\
  \    add(n8,n9,n17).\n\
  \    add(n9,n0,n9).\n\
  \    add(n9,n1,n10).\n\
  \    add(n9,n2,n11).\n\
  \    add(n9,n3,n12).\n\
  \    add(n9,n4,n13).\n\
  \    add(n9,n5,n14).\n\
  \    add(n9,n6,n15).\n\
  \    add(n9,n7,n16).\n\
  \    add(n9,n8,n17).\n\
  \    add(n9,n9,n18).\n\n\n\
  \    sub(n0,n0,n0).\n\
  \    sub(n0,n1,nm1).\n\
  \    sub(n0,n2,nm2).\n\
  \    sub(n0,n3,nm3).\n\
  \    sub(n0,n4,nm4).\n\
  \    sub(n0,n5,nm5).\n\
  \    sub(n0,n6,nm6).\n\
  \    sub(n0,n7,nm7).\n\
  \    sub(n0,n8,nm8).\n\
  \    sub(n0,n9,nm9).\n\
  \    sub(n1,n0,n1).\n\
  \    sub(n1,n1,n0).\n\
  \    sub(n1,n2,nm1).\n\
  \    sub(n1,n3,nm2).\n\
  \    sub(n1,n4,nm3).\n\
  \    sub(n1,n5,nm4).\n\
  \    sub(n1,n6,nm5).\n\
  \    sub(n1,n7,nm6).\n\
  \    sub(n1,n8,nm7).\n\
  \    sub(n1,n9,nm8).\n\
  \    sub(n2,n0,n2).\n\
  \    sub(n2,n1,n1).\n\
  \    sub(n2,n2,n0).\n\
  \    sub(n2,n3,nm1).\n\
  \    sub(n2,n4,nm2).\n\
  \    sub(n2,n5,nm3).\n\
  \    sub(n2,n6,nm4).\n\
  \    sub(n2,n7,nm5).\n\
  \    sub(n2,n8,nm6).\n\
  \    sub(n2,n9,nm7).\n\
  \    sub(n3,n0,n3).\n\
  \    sub(n3,n1,n2).\n\
  \    sub(n3,n2,n1).\n\
  \    sub(n3,n3,n0).\n\
  \    sub(n3,n4,nm1).\n\
  \    sub(n3,n5,nm2).\n\
  \    sub(n3,n6,nm3).\n\
  \    sub(n3,n7,nm4).\n\
  \    sub(n3,n8,nm5).\n\
  \    sub(n3,n9,nm6).\n\
  \    sub(n4,n0,n4).\n\
  \    sub(n4,n1,n3).\n\
  \    sub(n4,n2,n2).\n\
  \    sub(n4,n3,n1).\n\
  \    sub(n4,n4,n0).\n\
  \    sub(n4,n5,nm1).\n\
  \    sub(n4,n6,nm2).\n\
  \    sub(n4,n7,nm3).\n\
  \    sub(n4,n8,nm4).\n\
  \    sub(n4,n9,nm5).\n\
  \    sub(n5,n0,n5).\n\
  \    sub(n5,n1,n4).\n\
  \    sub(n5,n2,n3).\n\
  \    sub(n5,n3,n2).\n\
  \    sub(n5,n4,n1).\n\
  \    sub(n5,n5,n0).\n\
  \    sub(n5,n6,nm1).\n\
  \    sub(n5,n7,nm2).\n\
  \    sub(n5,n8,nm3).\n\
  \    sub(n5,n9,nm4).\n\
  \    sub(n6,n0,n6).\n\
  \    sub(n6,n1,n5).\n\
  \    sub(n6,n2,n4).\n\
  \    sub(n6,n3,n3).\n\
  \    sub(n6,n4,n2).\n\
  \    sub(n6,n5,n1).\n\
  \    sub(n6,n6,n0).\n\
  \    sub(n6,n7,nm1).\n\
  \    sub(n6,n8,nm2).\n\
  \    sub(n6,n9,nm3).\n\
  \    sub(n7,n0,n7).\n\
  \    sub(n7,n1,n6).\n\
  \    sub(n7,n2,n5).\n\
  \    sub(n7,n3,n4).\n\
  \    sub(n7,n4,n3).\n\
  \    sub(n7,n5,n2).\n\
  \    sub(n7,n6,n1).\n\
  \    sub(n7,n7,n0).\n\
  \    sub(n7,n8,nm1).\n\
  \    sub(n7,n9,nm2).\n\
  \    sub(n8,n0,n8).\n\
  \    sub(n8,n1,n7).\n\
  \    sub(n8,n2,n6).\n\
  \    sub(n8,n3,n5).\n\
  \    sub(n8,n4,n4).\n\
  \    sub(n8,n5,n3).\n\
  \    sub(n8,n6,n2).\n\
  \    sub(n8,n7,n1).\n\
  \    sub(n8,n8,n0).\n\
  \    sub(n8,n9,nm1).\n\
  \    sub(n9,n0,n9).\n\
  \    sub(n9,n1,n8).\n\
  \    sub(n9,n2,n7).\n\
  \    sub(n9,n3,n6).\n\
  \    sub(n9,n4,n5).\n\
  \    sub(n9,n5,n4).\n\
  \    sub(n9,n6,n3).\n\
  \    sub(n9,n7,n2).\n\
  \    sub(n9,n8,n1).\n\
  \    sub(n9,n9,n0).\n\n\n\
  \    take(c(H,T),H,T).\n\
  \    take(c(H,T),R,c(H,S)) :- take(T,R,S).\n\n\
  \    perm(eol,eol).\n\
  \    perm(X,c(H,T)) :- take(X,H,R), perm(R,T).\n\n\
  \    equals(X, X).\n\
  \    notequals(X, Y) :- equals(X, Y), !, fail.\n\
  \    notequals(X, Y).\n\n\
  \    checkPair(X,Y,Dist) :-\n\
  \    add(X,Dist,X1),\n\
  \    sub(X,Dist,X2),\n\
  \    notequals(Y, X1),\n\
  \    notequals(Y,X2).\n\n\
  \    checkHead(c(X,eol), Y).\n\n\n\
  \    checkHead(c(Q1,c(Q2,T)), Dist) :-\n\
  \    add(Dist,n1,Dist2),\n\
  \    checkPair(Q1,Q2, Dist2),\n\
  \    checkHead(c(Q1,T), Dist2).\n\n\
  \    checkDiags(c(X,eol)).\n\
  \    checkDiags(c(Q1,T)) :- checkHead(c(Q1,T),n0), checkDiags(T).\n\n\
   ?- perm(c(n1,c(n2,c(n3,c(n4,eol)))),R), checkDiags(R)"
