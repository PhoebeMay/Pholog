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





(* type 'a subst = Bottom of 'a term * 'a term | Top of 'a term * 'a term | Failure *)

(* type ('a, 'b) unifier = ('a , 'b) Hashtbl.t * ('a , 'b) Hashtbl.t *)


(* type ('a, 'b) unificationResult = Unifier of ('a , 'b) unifier | None *)

type unifRes = Success | Failure

let rec applyUnifier hashT term =
  match term with
  | TVar(_) -> ( try (
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

exception OHFUCK


let atomSubst s (Atom(p1, ts1)) =
  (* match s with
     | None -> print_string " FAIL "; raise OHFUCK
     | Some(subst) -> *)
  let res = Atom(p1, map (applyUnifier s) ts1)
  in res


let getClauses g hashT =
  let id = genFunctionId g
  in reverse (Hashtbl.find_all hashT id)

let rec unifyTermLists t1s t2s bottomSub topSub =
  match (t1s, t2s) with
  | ([],[]) -> Success
  | ([],_) -> Failure
  | (_,[]) -> Failure
  | (t1::t1ss, t2::t2ss) ->
    match unifyTerm (applyUnifier bottomSub  t1) (applyUnifier topSub t2) bottomSub topSub
    with
    | Success -> unifyTermLists t1ss t2ss bottomSub topSub
    | Failure -> Failure


and unifyTerm t1 t2 bottomSub topSub =
  match (t1,t2) with
  | (TVar(_), _) ->
    Hashtbl.add bottomSub t1 t2;
    Success

  | (_,TVar(_)) ->
    Hashtbl.add topSub t2 t1;
    Success

  | (TFun(funct1, ts1), TFun(funct2, ts2)) ->
    if funct1 = funct2
    then
      unifyTermLists ts1 ts2 bottomSub topSub
    else Failure



let unifyAtom  (Atom(p1, ts1)) (Atom(p2, ts2)) =
  if (len ts1) <> (len ts2)
  then None
  else if p1 <> p2
  then None
  else let bottomSub = Hashtbl.create 100
    in let topSub = Hashtbl.create 100
    in match unifyTermLists ts1 ts2 bottomSub topSub with
    | Success -> Some(bottomSub, topSub)
    | Failure -> None


let concat outer inner =
  let result = Hashtbl.create 10
  in let () = Hashtbl.iter
         (fun key value -> Hashtbl.add result key (applyUnifier outer value))
         inner
  in result

exception Feck

exception NoAnswer



let prove functionT goals =

  let genBase goals hash =
    let rec baseTs terms =
      match terms with
      | [] -> ()
      | TVar(v)::ts -> Hashtbl.replace hash (TVar(v)) (TVar(v)); baseTs ts
      | TFun(_,ts)::tss -> baseTs ts; baseTs tss
    in let _ = map (fun (Atom(_pred,terms)) -> baseTs terms) goals
    in hash

  in let rec backTrack bts =
    match bts with
    | (goal,clauses,subst)::bs -> proveGoal goal clauses subst bs
    | [] -> None

  and proveGoal goal clauses s bts  =
    match clauses with
    | [] -> backTrack bts
    | ((Clause(head, elems))::cs) ->
      let other = (goal, cs, s)
      in let newBackTrackStack = other::bts
      in match unifyAtom goal head with
      | None -> backTrack newBackTrackStack
      | Some(bottomSub, topSub) ->
          let newGoalsInsideChosenClause = map (atomSubst topSub) elems
          in let newB = concat topSub bottomSub
          in let substNewGoalsQ =  proveAll newGoalsInsideChosenClause newB newBackTrackStack
          in substNewGoalsQ

  and proveAll goals st bts =
    match goals with
    |  [] -> Some(st, bts)
    | (g::gs) ->

      let id = genFunctionId g
      in let clauses = reverse (Hashtbl.find_all functionT id)
      in let subst = proveGoal g clauses st []
      in tryProof subst bts st gs

  and tryProof subst bts s gs =
        match subst with
        | None -> backTrack bts
        | Some(substNewGoalsQ, _btsNew) ->
          let remainingGoals = (map (fun atom -> atomSubst substNewGoalsQ atom )) gs
          in let newsub = concat substNewGoalsQ s
          in match proveAll remainingGoals newsub bts
          with | Some(s) -> Some(s)
               | None -> tryProof (backTrack _btsNew) bts s gs

    in let base = genBase goals (Hashtbl.create 100)
    in proveAll goals base []



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
    in let s = prove hashT goals
    in match s with
    | Some(subst, _) ->
      let res = map (fun g -> atomSubst subst g) goals
      in
      Ans(Resolvant(map genStringAtom res))
    | None -> Fail
  )
  with False -> Fail
