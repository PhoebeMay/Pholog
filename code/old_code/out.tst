Abstract clause is 
(ParseTree.Clause (
   (ParseTree.Atom ("animal", [(ParseTree.TFun ("craig", []))])), []))
1
beybey
Abstract clause is 
(ParseTree.Clause (
   (ParseTree.Atom ("animal", [(ParseTree.TFun ("phoebe", []))])), []))
1
beybey
Abstract clause is 
(ParseTree.Clause (
   (ParseTree.Atom ("animal", [(ParseTree.TFun ("dusk", []))])), []))
1
beybey
Abstract clause is 
(ParseTree.Clause ((ParseTree.Atom ("dog", [(ParseTree.TFun ("dusk", []))])),
   []))
1
beybey
Function table is 
(((AbstractF(animal 1))(((Clause(Atom animal((TFun dusk())))())0)((Clause(Atom animal((TFun phoebe())))())0)((Clause(Atom animal((TFun craig())))())0)))((AbstractF(dog 1))(((Clause(Atom dog((TFun dusk())))())0))))

(Dt.AbstractF ("dog", 1)) -> 
[((ParseTree.Clause (
                                   (ParseTree.Atom ("dog",
                                      [(ParseTree.TFun ("dusk", []))])),
                                   [])),
                                0)]
(Dt.AbstractF ("animal", 1)) -> 
[((ParseTree.Clause (
                                      (ParseTree.Atom ("animal",
                                         [(ParseTree.TFun ("dusk", []))])),
                                      [])),
                                   0);
                                   ((ParseTree.Clause (
                                       (ParseTree.Atom ("animal",
                                          [(ParseTree.TFun ("phoebe", []))])),
                                       [])),
                                    0);
                                   ((ParseTree.Clause (
                                       (ParseTree.Atom ("animal",
                                          [(ParseTree.TFun ("craig", []))])),
                                       [])),
                                    0)
Gen code now
code clause with set ()
Clause is (ParseTree.Clause ((ParseTree.Atom ("dog", [(ParseTree.TFun ("dusk", []))])),
   []))
code clause with set ()
Clause is (ParseTree.Clause (
   (ParseTree.Atom ("animal", [(ParseTree.TFun ("craig", []))])), []))
code clause with set ()
Clause is (ParseTree.Clause (
   (ParseTree.Atom ("animal", [(ParseTree.TFun ("phoebe", []))])), []))
code clause with set ()
Clause is (ParseTree.Clause (
   (ParseTree.Atom ("animal", [(ParseTree.TFun ("dusk", []))])), []))
seen false
seen true
[((Dt.AbstractF ("query", 0)),
  [(0,
    [(Dt.Allocate 1); (Dt.PutVariable ((Dt.Env 0), (Dt.Arg 0)));
      (Dt.Call (Dt.AbstractF ("animal", 1)));
      (Dt.PutValue ((Dt.Env 0), (Dt.Arg 0)));
      (Dt.Call (Dt.AbstractF ("dog", 1))); Dt.Return])
    ]);
  ((Dt.AbstractF ("dog", 1)),
   [(0,
     [(Dt.Allocate 0); (Dt.GetStructureA (("dusk", 0), (Dt.Arg 0)));
       Dt.Proceed; Dt.Deallocate])
     ]);
  ((Dt.AbstractF ("animal", 1)),
   [(0,
     [(Dt.TryMeElse (Dt.AbstractC ((Dt.AbstractF ("animal", 1)), 1)));
       (Dt.Allocate 0); (Dt.GetStructureA (("craig", 0), (Dt.Arg 0)));
       Dt.Proceed; Dt.Deallocate]);
     (1,
      [(Dt.RetryMeElse (Dt.AbstractC ((Dt.AbstractF ("animal", 1)), 2)));
        (Dt.Allocate 0); (Dt.GetStructureA (("phoebe", 0), (Dt.Arg 0)));
        Dt.Proceed; Dt.Deallocate]);
     (2,
      [Dt.TrustMe; (Dt.Allocate 0);
        (Dt.GetStructureA (("dusk", 0), (Dt.Arg 0))); Dt.Proceed;
        Dt.Deallocate])
     ])
  ]
flaaat cooooooode
0. (Dt.Allocate 1)
1. (Dt.PutVariable ((Dt.Env 0), (Dt.Arg 0)))
2. (Dt.Call (Dt.PositionF 10))
3. (Dt.PutValue ((Dt.Env 0), (Dt.Arg 0)))
4. (Dt.Call (Dt.PositionF 6))
5. Dt.Return
6. (Dt.Allocate 0)
7. (Dt.GetStructureA (("dusk", 0), (Dt.Arg 0)))
8. Dt.Proceed
9. Dt.Deallocate
10. (Dt.TryMeElse (Dt.PositionC 15))
11. (Dt.Allocate 0)
12. (Dt.GetStructureA (("craig", 0), (Dt.Arg 0)))
13. Dt.Proceed
14. Dt.Deallocate
15. (Dt.RetryMeElse (Dt.PositionC 20))
16. (Dt.Allocate 0)
17. (Dt.GetStructureA (("phoebe", 0), (Dt.Arg 0)))
18. Dt.Proceed
19. Dt.Deallocate
20. Dt.TrustMe
21. (Dt.Allocate 0)
22. (Dt.GetStructureA (("dusk", 0), (Dt.Arg 0)))
23. Dt.Proceed
24. Dt.Deallocate

begin execute
(Dt.Allocate 1)
(Dt.PutVariable ((Dt.Env 0), (Dt.Arg 0)))
(Dt.Call (Dt.PositionF 10))
(Dt.TryMeElse (Dt.PositionC 15))
(Dt.Allocate 0)
(Dt.GetStructureA (("craig", 0), (Dt.Arg 0)))
get structure a
i am here
(ExecuteCode.HeapPointer (ExecuteCode.B ref (ExecuteCode.UnboundVar)))
match argvar!!
211
215
binding
Dt.Proceed
Dt.Deallocate
Code pointer is 14 return address is 3
(Dt.PutValue ((Dt.Env 0), (Dt.Arg 0)))
(Dt.Call (Dt.PositionF 6))
(Dt.Allocate 0)
(Dt.GetStructureA (("dusk", 0), (Dt.Arg 0)))
get structure a
i am here
(ExecuteCode.HeapPointer
   (ExecuteCode.B ref ((ExecuteCode.StrPointer ("craig", [||])))))
match argvar!!
211
229
backtrack
backtrack from rewind trail
ref ([(ExecuteCode.Var ref ((ExecuteCode.StrPointer ("craig", [||]))));
       ExecuteCode.Label; ExecuteCode.Label])
unbinding
backtrack from rewind trail
ref ([ExecuteCode.Label; ExecuteCode.Label])
finished backtracking
jump to code pointer 15
(Dt.RetryMeElse (Dt.PositionC 20))
(Dt.Allocate 0)
(Dt.GetStructureA (("phoebe", 0), (Dt.Arg 0)))
get structure a
i am here
(ExecuteCode.HeapPointer (ExecuteCode.B ref (ExecuteCode.UnboundVar)))
match argvar!!
211
215
binding
Dt.Proceed
Dt.Deallocate
Code pointer is 19 return address is 5
Dt.Return
Return
{ ExecuteCode.vars =
  [|(ExecuteCode.HeapPointer
       (ExecuteCode.B ref ((ExecuteCode.StrPointer ("phoebe", [||])))))
    |];
  returnAddress = -1 }
finish execute
[(Runall.HeapPointerF (Runall.BF (Runall.StrPointerF ("phoebe", [||]))))]




                                   ]