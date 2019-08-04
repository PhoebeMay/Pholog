diff(r,g).
diff(r,b).
diff(r,y).
diff(g,r).
diff(g,b).
diff(g,y).
diff(b,r).
diff(b,y).
diff(b,g).
diff(y,r).
diff(y,b).
diff(y,g).

yellow(y).
green(g).

valid(X0,X1,X2,X3,X4,X5,X6,X7,X8,
         X9,X10,X11,X12,X13,X14,
         X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,
         X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,
         X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49
) :-
diff(X1,X2),
diff(X2,X3),
diff(X4,X5),
diff(X6,X7),
diff(X8,X9),
diff(X10,X11),
diff(X11,X12),
diff(X12,X13),
yellow(X1),
green(X2).

goal :- valid(X0,X1,X2,X3,X4,X5,X6,X7,X8,
         X9,X10,X11,X12,X13,X14,
         X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,
         X25,X26,X27,X28,X29,X30,X31,X32,X33,X34,X35,X36,
         X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49).

:- initialization(goal).
