open ParseTree

type placeholder = Argument of int | Inner of int

let rec map f xs =
  match xs with
  | (y::ys) ->
    f y :: map f ys
  | [] -> []

let genStringPlaceholder p =
  match p with
  | Argument(n) -> "Argument: "^ string_of_int n
  | Inner(n) -> "Inner: "^ string_of_int n

let rec genStringTerm term =
  match term with
  | TVar(p) -> TVar(genStringPlaceholder p)
  | TFun(f, ts) -> TFun(f, map genStringTerm ts)

let genStringAtom (Atom(pred, ts)) = (Atom(pred, map genStringTerm ts))

let genStringClause (Clause(at, ats)) = (Clause(genStringAtom at, map genStringAtom ats))

let setIfNotAlready hashT key value =
  try (
    let _ = Hashtbl.find hashT key
    in ()
  )
  with Not_found ->
    Hashtbl.add hashT key value;
    ()

(*Map from string variable to placeholder*)
let genArgMap (ag : string term list) =
  let rec genArgumentMapH arguments hashT counter =
    match arguments with
    | (x::xs) ->
      (match x with
       | TVar(var) ->
         let arg = Argument(counter)
         in
         (* Printf.printf "Arg map plus %s -> %s" var (genStringPlaceholder arg);
            print_endline ""; *)
         setIfNotAlready hashT var arg;
         genArgumentMapH xs hashT (counter + 1)
       | TFun(_, ts) ->
         let (newT,newC) = genArgumentMapH ts hashT counter
         in genArgumentMapH xs newT newC
      )
    | [] -> (hashT, counter)
  in let (table, _) =  genArgumentMapH ag (Hashtbl.create 100) 0
  in table

let rec applyArgMapToTerm argmap count term =
  match term with
  | TVar(var) -> (
      (* Printf.printf "Lookup %s \n" var; *)
      try (
        let found = Hashtbl.find argmap var
        in
        (* print_endline "argument var"; *)
        TVar(found)
      )
      with Not_found -> (
          (* print_endline "inner var"; *)

          let newVar = Inner(!count)
          in Hashtbl.add argmap var newVar;
          count := !count + 1;
          TVar(newVar)
        )
    )
  | TFun(name, ts) -> TFun(name, map (applyArgMapToTerm argmap count) ts)

let makeGoalsAbstract gs =
  let argmap = Hashtbl.create 100
  in let count = ref 0
  in map (fun (Atom(p,ts)) ->  Atom(p, map (applyArgMapToTerm argmap count) ts)) gs

let makeClauseAbstract (Clause(Atom(name, args),tail)) =
  let argmap =
    (*
print_endline (strClause (Clause(Atom(name, args),tail))) ; *)
    genArgMap args
  in let count =
       (* print_endline "finished argmap for this clause";  *)
       ref 0

  in let appAtom (Atom(pred, terms)) = Atom(pred, map (applyArgMapToTerm argmap count) terms)
  in let appClause (Clause(head, tail)) = Clause(appAtom head, map appAtom tail)
  in appClause (Clause(Atom(name, args),tail))

let makeProgramAbstract (Program(Sentence(clauses), Resolvant(goals))) =
  let newClauses = map makeClauseAbstract clauses
  in let newGoals = makeGoalsAbstract goals
  in let _strC =  (strSentence (Sentence(map genStringClause newClauses)))
  in let _strG = (strResolvant (Resolvant(map genStringAtom newGoals)))
  in
  (* print_endline "Abstracted program is:";
     print_endline "";
     print_endline _strC;
     print_endline "";
     print_endline _strG;
     print_endline ""; *)
  (Program(Sentence(newClauses), Resolvant(newGoals)))



exception UnifiyFail
exception False

type functionId = string * int

(*Could also do this counting in parser*)
let rec len vals =
  match vals with
  | (_::xs) -> 1 + len xs
  | [] -> 0

let reverse ls =
  let rec reverseH vals res =
    match vals with
    | [] -> res
    | (x::xs) -> reverseH xs (x::res)
  in reverseH ls []

let genFunctionId (Atom(name, args)) = (name, len args)

let genFunctionTable (Sentence(cls)) =
  let rec genFunctionTableH clauses hashT =
    match clauses with
    | [] -> hashT
    | (Clause(head, body)::cs) ->
      Hashtbl.add hashT (genFunctionId head) (Clause(head, body));
      genFunctionTableH cs hashT
  in genFunctionTableH cls (Hashtbl.create 100)


let rec map f xs =
  match xs with
  | (y::ys) ->
    f y :: map f ys
  | [] -> []

let printUnifier hashT =
  print_endline "The unifier is:";
  Hashtbl.fold (fun key value _ -> Printf.printf "%s -> %s\n" (strTerm (genStringTerm key)) (strTerm (genStringTerm value))) hashT ();
  print_endline ""


type 'a subst = Bottom of 'a term * 'a term | Top of 'a term * 'a term


let rec applyUnifier hashT term =
  match term with
  | TVar(_) -> (
      try (
        Hashtbl.find hashT term
      )
      with Not_found -> term
    )
  | TFun(name,ts) -> TFun(name, map (applyUnifier hashT) ts)



let rec applyUnifierExn hashT term =
  match term with
  | TVar(_) -> (
      Hashtbl.find hashT term
    )
  | TFun(name,ts) -> TFun(name, map (applyUnifierExn hashT) ts)

let atomSubst subst (Atom(p1, ts1)) =
  let res =
    (* Printf.printf "Atom subst %s \n" (strAtom ( genStringAtom (Atom(p1, ts1)))); *)
    Atom(p1, map (applyUnifier subst) ts1)
  in
  (* Printf.printf "Atom subst gives %s \n\n" (strAtom (genStringAtom res)); *)
  res





let getClauses g hashT =
  let id = genFunctionId g
  in reverse (Hashtbl.find_all hashT id)

let rec unifyTermLists t1s t2s bottomSub topSub =
  match (t1s, t2s) with
  | ([],[]) -> ()
  | ([],_) -> raise False
  | (_,[]) -> raise False
  | (t1::t1ss, t2::t2ss) ->
    let () = unifyTerm (applyUnifier bottomSub  t1) (applyUnifier topSub t2) bottomSub topSub;
    in unifyTermLists t1ss t2ss bottomSub topSub

and unifyTerm t1 t2 bottomSub topSub =
  match (t1,t2) with
  | (TVar(_), _) ->
    (* Printf.printf "Create subst unifyTerm 1: [%s/%s]\n" (strTerm (genStringTerm t2)) (strTerm (genStringTerm t1)); *)
    (* print_endline ""; *)
    Hashtbl.add bottomSub t1 t2
  (* Bottom(t1, t2) *)

  | (_,TVar(_)) ->
    (* print_endline "Create subst unifyTerm 2\n"; *)
    (* Printf.printf "Create subst unifyTerm 2: [%s/%s]\n" (strTerm (genStringTerm t1)) (strTerm (genStringTerm t2)); *)
    (* print_endline ""; *)
    Hashtbl.add topSub t2 t1
  (* Top(t2, t1) *)

  | (TFun(funct1, ts1), TFun(funct2, ts2)) ->
    (* Printf.printf "Create subst unifyTerm 3 check %s %s \n" funct1 funct2; *)
    (* print_endline ""; *)
    if funct1 = funct2
    then
      unifyTermLists ts1 ts2 bottomSub topSub

    else (raise False)



let unifyAtom  (Atom(p1, ts1)) (Atom(p2, ts2)) =
  if (len ts1) <> (len ts2)
  then raise False
  else if p1 <> p2
  then raise False
  else let bottomSub = Hashtbl.create 100
    in let topSub = Hashtbl.create 100
    in let () = unifyTermLists ts1 ts2 bottomSub topSub
    in (bottomSub, topSub)

let concat outer inner =
  let () =
    print_endline "concat";
    print_endline "inner:";
    printUnifier inner;
    print_endline "outer:";
    printUnifier outer;
    ()
  in let result = Hashtbl.create 10
  in let () = Hashtbl.iter
         (fun key value -> Hashtbl.add result key (applyUnifier outer value))
         inner
  in
  print_endline "result:";
  printUnifier result;
  result

exception Feck

let count = ref 0

let backTrack = fun () -> print_endline "backtrack!"

let rec prove currentConsideredClauses functionT goals s =
  count := !count + 1;
  match goals with
  |  [] -> raise Feck
  | (g::gs) ->
    match currentConsideredClauses with
    | [] ->  raise False
    | ((Clause(head, elems))::cs) ->
      try(
        let (bottomSub, topSub) =
          Printf.printf "The goal is: %s \n" (strAtom (genStringAtom g));
          Printf.printf "The clause is: %s\n\n" (strClause (genStringClause(Clause(head, elems))));
          print_endline "The inital subst is";
          printUnifier s;
          unifyAtom g head

        in let () =
             print_endline "bottom sub";
             printUnifier bottomSub;
             print_endline "top sub";
             printUnifier topSub;
             ()

        in let newGoalsInsideChosenClause = map (atomSubst topSub) elems

        in let newB = concat topSub bottomSub

        in let substProveNewGoals =  match newGoalsInsideChosenClause with
            | [] -> newB
            | (x::_) ->
              let id = genFunctionId x
              in let clauses = reverse (Hashtbl.find_all functionT id)
              in  prove clauses functionT newGoalsInsideChosenClause newB

        in let () =
             print_endline "subst prove new goals";
             printUnifier substProveNewGoals;
             ()



        (* in let remainingGoals = (map (fun atom -> atomSubst proveThisGoal atom )) gs *)

        in let remainingGoals = (map (fun atom -> atomSubst substProveNewGoals atom )) gs

        in let () =
             print_endline "remainingGoals";
             print_endline (String.concat " "(map (fun atom -> strAtom (genStringAtom atom)) remainingGoals))

        in  let () =
              Printf.printf "The goal was: %s \n" (strAtom (genStringAtom g));
              Printf.printf "The clause was: %s\n\n" (strClause (genStringClause(Clause(head, elems))));
              print_endline "The inital subst was";
              printUnifier s
        in match remainingGoals with
        | [] ->
          print_endline "match rg 1";
          concat substProveNewGoals s
        | (x::_) ->
          let id =  print_endline "march rg 2"; genFunctionId x
          in let clauses = reverse (Hashtbl.find_all functionT id)
          in  prove clauses functionT remainingGoals (concat substProveNewGoals s)

      )
      with False ->  print_endline "backtrack!";
        (prove cs functionT goals s)

let genBase goals hash =
  let rec baseTs terms =
    match terms with
    | [] -> ()
    | TVar(v)::ts -> Hashtbl.replace hash (TVar(v)) (TVar(v)); baseTs ts
    | TFun(_,ts)::tss -> baseTs ts; baseTs tss
  in let _ = map (fun (Atom(_pred,terms)) -> baseTs terms) goals
  in hash


let interpret prog =
  try (
    (* print_endline "interpret"; *)
    let (Program(Sentence(clauses), Resolvant(goals))) = makeProgramAbstract prog
    in let hashT = genFunctionTable (Sentence(clauses))
    in let subst = match goals with
        | (g::_) ->
          let id = genFunctionId g
          in let clauses = reverse (Hashtbl.find_all hashT id)
          in let base = genBase goals (Hashtbl.create 100)
          in
          (* print_endline "base is";
             printUnifier base; *)
          prove clauses hashT goals base
        | [] -> Hashtbl.create 0
    in let () =
         (* printUnifier subst; *)
         ()
    in let res = map (fun g -> atomSubst subst g) goals
    in
    print_endline (string_of_int !count);
    Ans(Resolvant(map genStringAtom res))
  )
  with False -> Fail


let () = print_endline (string_of_int !count)
