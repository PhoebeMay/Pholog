open ParseTree
open Logging

type placeholder = Argument of int | Inner of int [@@deriving show]

let rec map f xs = match xs with y :: ys -> f y :: map f ys | [] -> []

let setIfNotAlready hashT key value =
  try
    let _ = Hashtbl.find hashT key in
    ()
  with Not_found ->
    Hashtbl.add hashT key value;
    ()

(*Map from string variable to placeholder*)
let genArgMap ag =
  let rec genArgumentMapH arguments hashT counter =
    match arguments with
    | x :: xs -> (
        match x with
        | TVar var ->
            let arg = Argument counter in
            (* Printf.printf "Arg map plus %s -> %s" var (genStringPlaceholder arg);
            print_endline ""; *)
            setIfNotAlready hashT var arg;
            genArgumentMapH xs hashT (counter + 1)
        | TFun (_, ts) ->
            let newT, newC = genArgumentMapH ts hashT counter in
            genArgumentMapH xs newT newC )
    | [] -> (hashT, counter)
  in
  let table, _ = genArgumentMapH ag (Hashtbl.create 100) 0 in
  table

let rec applyArgMapToTerm argmap count term =
  match term with
  | TVar var -> (
      try
        (* Printf.printf "Lookup %s \n" var; *)
        let found = Hashtbl.find argmap var in
        (* print_endline "argument var"; *)
        TVar found
      with Not_found ->
        (* print_endline "inner var"; *)
        let newVar = Inner !count in
        Hashtbl.add argmap var newVar;
        count := !count + 1;
        TVar newVar )
  | TFun (name, ts) -> TFun (name, map (applyArgMapToTerm argmap count) ts)

let makeGoalsAbstract gs =
  let argmap = Hashtbl.create 100 in
  let count = ref 0 in
  map
    (fun (Atom (p, ts)) -> Atom (p, map (applyArgMapToTerm argmap count) ts))
    gs

let makeClauseAbstract (Clause (Atom (name, args), tail)) =
  let argmap =
    (*
print_endline (strClause (Clause(Atom(name, args),tail))) ; *)
    genArgMap args
  in
  let count =
    (* print_endline "finished argmap for this clause";  *)
    ref 0
  in
  let appAtom (Atom (pred, terms)) =
    Atom (pred, map (applyArgMapToTerm argmap count) terms)
  in
  let appClause (Clause (head, tail)) =
    Clause (appAtom head, map appAtom tail)
  in
  appClause (Clause (Atom (name, args), tail))

let makeProgramAbstract (Program (Sentence clauses, Resolvant goals)) =
  let newClauses = map makeClauseAbstract clauses in
  let newGoals = makeGoalsAbstract goals in
  Program (Sentence newClauses, Resolvant newGoals)

type functionId = string * int [@@deriving show]

type label = ClausePos of int | Fail [@@deriving show]

(*No allocate or deallocate because i am allocating for every call rn*)

exception UnifiyFail

exception False

(*Could also do this counting in parser*)
let rec len vals = match vals with _ :: xs -> 1 + len xs | [] -> 0

let reverse ls =
  let rec reverseH vals res =
    match vals with [] -> res | x :: xs -> reverseH xs (x :: res)
  in
  reverseH ls []

let genFunctionId (Atom (name, args)) = (name, len args)

let genFunctionTable (Sentence cls) =
  let rec genFunctionTableH clauses hashT =
    match clauses with
    | [] -> hashT
    | Clause (head, body) :: cs ->
        Hashtbl.add hashT (genFunctionId head) (Clause (head, body));
        genFunctionTableH cs hashT
  in
  genFunctionTableH cls (Hashtbl.create 100)

let rec map f xs = match xs with y :: ys -> f y :: map f ys | [] -> []

let printUnifier hashT =
  print_endline "The unifier is:";
  Hashtbl.fold
    (fun key value _ ->
      Printf.printf "%s -> %s\n"
        (show_term pp_placeholder key)
        (show_term pp_placeholder value))
    hashT ();
  print_endline ""

exception Oops

type unifRes = Success | Failure

let rec applyUnifier hashT term =
  match term with
  | TVar _ -> ( try Hashtbl.find hashT term with Not_found -> term )
  | TFun (name, ts) -> TFun (name, map (applyUnifier hashT) ts)

let rec applyUnifierExn hashT term =
  match term with
  | TVar _ -> Hashtbl.find hashT term
  | TFun (name, ts) -> TFun (name, map (applyUnifierExn hashT) ts)

exception OHFUCK

let atomSubst s (Atom (p1, ts1)) =
  let res = Atom (p1, map (applyUnifier s) ts1) in
  res

let getClauses g hashT =
  let id = genFunctionId g in
  reverse (Hashtbl.find_all hashT id)

let rec unifyTermLists t1s t2s bottomSub topSub =
  match (t1s, t2s) with
  | [], [] -> Success
  | [], _ -> Failure
  | _, [] -> Failure
  | t1 :: t1ss, t2 :: t2ss -> (
      match
        unifyTerm
          (applyUnifier bottomSub t1)
          (applyUnifier topSub t2) bottomSub topSub
      with
      | Success -> unifyTermLists t1ss t2ss bottomSub topSub
      | Failure -> Failure )

and unifyTerm t1 t2 bottomSub topSub =
  match (t1, t2) with
  | TVar _, _ ->
      Hashtbl.add bottomSub t1 t2;
      Success
  | _, TVar _ ->
      Hashtbl.add topSub t2 t1;
      Success
  | TFun (funct1, ts1), TFun (funct2, ts2) ->
      if funct1 = funct2 then unifyTermLists ts1 ts2 bottomSub topSub
      else Failure

let unifyAtom (Atom (p1, ts1)) (Atom (p2, ts2)) =
  if len ts1 <> len ts2 then None
  else if p1 <> p2 then None
  else
    let bottomSub = Hashtbl.create 100 in
    let topSub = Hashtbl.create 100 in
    match unifyTermLists ts1 ts2 bottomSub topSub with
    | Success -> Some (bottomSub, topSub)
    | Failure -> None

let concat outer inner =
  let result = Hashtbl.create 10 in
  let () =
    Hashtbl.iter
      (fun key value -> Hashtbl.add result key (applyUnifier outer value))
      inner
  in
  result

(* type bts = (placeholder atom * placeholder clause list * ((placeholder term, placeholder term) Hashtbl.t)) list [@@deriving show] *)

type ab = placeholder atom * placeholder clause list [@@deriving show]

type c = (placeholder term * placeholder term) list [@@deriving show]

type d = string list [@@deriving show]

let strbtselem (a, b, c) =
  let newc = List.of_seq (Hashtbl.to_seq c) in
  show_ab (a, b) ^ show_c newc

type pal = placeholder atom list [@@deriving show]

let strbts x = show_d (List.map strbtselem x)

type env =
  | E of
      placeholder atom list
      * (placeholder term, placeholder term) Hashtbl.t
      * env ref
  | Base

exception Backtrack

let prove functionT goals =
  let genBase goals hash =
    let rec baseTs terms =
      match terms with
      | [] -> ()
      | TVar v :: ts ->
          Hashtbl.replace hash (TVar v) (TVar v);
          baseTs ts
      | TFun (_, ts) :: tss ->
          baseTs ts;
          baseTs tss
    in
    let _ = map (fun (Atom (_pred, terms)) -> baseTs terms) goals in
    hash
  in
  let rec proveAll goals clauses s =
    (* let () = logDebug (fun m -> m "prove goals  %a" (pp_pal) goals) in *)
    match goals with
    | [] -> s
    | g :: gs -> (
        try
          let () =
            logDebug (fun m -> m "prove goal  %a" (pp_atom pp_placeholder) g)
          in
          (* get substitution for g *)
          let subst =
            match clauses with
            | [] -> raise Backtrack
            | Clause (head, elems) :: _ -> (
                match unifyAtom g head with
                | None -> raise Backtrack
                | Some (bottomSub, topSub) -> (
                    let newGoalsInsideChosenClause =
                      map (atomSubst topSub) elems
                    in
                    let newS = concat topSub bottomSub in
                    match newGoalsInsideChosenClause with
                    | [] ->
                        (* let () = logDebug (fun m -> m "new goals are empty") *)
                        (* in *)
                        (* TODO WHAT SUB *)
                        (* let () = logDebug (fun m -> m "not after ngs") in *)
                        newS
                    | x :: _ ->
                        let id = genFunctionId x in
                        let nclauses =
                          reverse (Hashtbl.find_all functionT id)
                        in
                        let s =
                          proveAll newGoalsInsideChosenClause nclauses newS
                          (* in let () =  printUnifier newS *)
                          (* in let () =  printUnifier bottomSub *)
                          (* in let () =  printUnifier topSub *)
                          (* in let () = logDebug (fun m -> m "after ngs") *)
                        in
                        s ) )
          in
          (* let ()  = printUnifier subst in *)
          let () =
            logDebug (fun m ->
                m " translated goal is  %a" (pp_atom pp_placeholder)
                  (atomSubst subst g))
          in
          let remainingGoals = (map (fun atom -> atomSubst subst atom)) gs in
          (* let () = logDebug (fun m -> m "remainingGoals  %a" (pp_pal) remainingGoals) *)
          let newsub = concat subst s in
          match remainingGoals with
          | [] -> newsub
          | x :: _ ->
              let id = genFunctionId x in
              let nclauses = reverse (Hashtbl.find_all functionT id) in
              proveAll remainingGoals nclauses newsub
        with Backtrack -> (
          match clauses with
          | [] -> raise Backtrack
          | _ :: cs ->
              let () = logDebug (fun m -> m "backtrack to next goal") in
              proveAll goals cs s ) )
    (* proveAll remainingGoals newsub *)

    (*
  and getClauseGoals goal clauses s bts  : ('e * 'f * 'g)option=
    match clauses with
    | [] -> None
    | ((Clause(head, elems))::cs) ->
      let other = (goal, cs, s)
      in let newBackTrackStack = other::bts
      in match unifyAtom goal head with
      | None -> getClauseGoals goal cs s bts
      | Some(bottomSub, topSub) ->
        let newGoalsInsideChosenClause = map (atomSubst topSub) elems
        in let newsub = concat topSub bottomSub
        in Some(newGoalsInsideChosenClause, newsub, newBackTrackStack)
  (* in let (substNewGoalsQ,resBts) =  proveAll newGoalsInsideChosenClause newsub []


     in (substNewGoalsQ,newBackTrackStack) *)

  and proveGoal (goal : placeholder atom) clauses (s) bts : ('a * 'd) option=
    let newgoals = getClauseGoals goal clauses s []
    in let () = logDebug (fun m -> m "prove goal  %a" (pp_atom pp_placeholder) goal)
    in let () = print_endline ("bts "^ (strbts bts))
    in match newgoals with
    | Some(ngs,ns,nb) ->
      let newsub = concat ns s
      in proveAll ngs newsub nb
    | None -> backTrack bts

  and proveAll goals st bts  : ('b * 'c ) option=
    match goals with
    |  [] -> Some(st, bts)
    | (g::gs) ->
      let id = genFunctionId g
      in let clauses = reverse (Hashtbl.find_all functionT id)
      in match proveGoal g clauses st []
      with None -> backTrack bts
         | Some(ns,nbts) ->
           let ngs = map (atomSubst ns) gs
           in proveAll ngs ns nbts *)

    (* and tryProof subst bts s gs =
     match subst with
     | None -> backTrack bts
     | Some(substNewGoalsQ) ->
      let remainingGoals = (map (fun atom -> atomSubst substNewGoalsQ atom )) gs
      in let newsub = concat substNewGoalsQ s
      in proveAll remainingGoals newsub bts
      (* with | (Some(s),b) -> (s,b)
           | (None,b) ->
             let (s,btsnew) = (backTrack b)
             in *)
             tryProof s btsnew s gs *)
  in
  let base = genBase goals (Hashtbl.create 100) in
  match goals with
  | [] -> Some base
  | x :: _ -> (
      let id = genFunctionId x in
      let nclauses = reverse (Hashtbl.find_all functionT id) in
      try Some (proveAll goals nclauses base) with Backtrack -> None )

let genBase goals hash =
  let rec baseTs terms =
    match terms with
    | [] -> ()
    | TVar v :: ts ->
        Hashtbl.replace hash (TVar v) (TVar v);
        baseTs ts
    | TFun (_, ts) :: tss ->
        baseTs ts;
        baseTs tss
  in
  let _ = map (fun (Atom (_pred, terms)) -> baseTs terms) goals in
  hash

let rec genStringTerm term =
  match term with
  | TVar p -> TVar (show_placeholder p)
  | TFun (f, ts) -> TFun (f, map genStringTerm ts)

let genStringAtom (Atom (pred, ts)) = Atom (pred, map genStringTerm ts)

let interpret prog =
  print_endline "interpret";
  let (Program (Sentence clauses, Resolvant goals)) =
    makeProgramAbstract prog
  in
  let hashT = genFunctionTable (Sentence clauses) in
  let s = prove hashT goals in
  match s with
  | Some subst ->
      let res = map (fun g -> atomSubst subst g) goals in
      Ans (Resolvant (map genStringAtom res))
  | None -> Fail
