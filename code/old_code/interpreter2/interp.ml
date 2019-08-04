open ParseTree

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

let rec mapTerms f xs =
  match xs with
  | (y::ys) ->
    let res = f y
    in res :: mapTerms f ys
  | [] -> []

let rec map f xs =
  match xs with
  | (y::ys) ->
    f y :: map f ys
  | [] -> []

let rec unifyTermLists t1s t2s =
  match (t1s, t2s) with
  | ([],[]) -> fun x -> x
  | ([],_) -> raise False
  | (_,[]) -> raise False
  | (t1::t1ss, t2::t2ss) ->
    let unifier = unifyTerm t1 t2
    in let unifyRest = unifyTermLists (map unifier t1ss) (map unifier t2ss)
    in fun x ->  unifyRest (unifier x)

and unifyTerm t1 t2 =
  match (t1,t2) with
  | (TVar(_), _) ->
    fun x ->
      if x = t1
      then t2
      else x
  | (_,TVar(_)) ->
    fun x -> if x = t2 then t1 else x
  | (TFun(funct1, ts1), TFun(funct2, ts2)) ->
    if funct1 = funct2
    then unifyTermLists ts1 ts2
    else (raise False)

let unifyAtom (Atom(p1, ts1)) (Atom(p2, ts2))  =
  if (len ts1) <> (len ts2)
  then raise False
  else if p1 <> p2
  then raise False
  else unifyTermLists ts1 ts2

let atomSubst subst (Atom(p1, ts1)) = Atom(p1, mapTerms subst ts1)


(*
    let rec proveBody subst body allClauses =
      match body with
      | [] -> subst
      | (b::bs) ->
        let new_subst = prove allClauses allClauses (atomSubst subst b) in
        proveBody (fun x -> new_subst (subst x)) bs allClauses
*)

(* Evauluates a function call. Returns a substitution, or raises False*)

    (*
let rec evalFun clauses goal hashT s =
  match clauses with
  | [] -> raise False
  | (Clause(head,body)::cs) ->
    try (
      let subst = unifyAtom head goal
      in let newBody = map (atomSubst subst) body
      in prove hashT newBody (fun x -> subst (s x))
    )
    with False -> evalFun cs goal hashT s
*)

      (*
let rec proveGoal (Clause(head,body)) goal hashT s =
  let subst = unifyAtom head goal
  in let newBody = map (atomSubst subst) body
  in prove hashT newBody (fun x -> subst (s x))
*)

let rec prove currentConsideredClauses hashTable goals s =
  match goals with
  |  [] -> s
  | (g::gs) ->
    match currentConsideredClauses with
    | [] -> raise False
    | ((Clause(head, elems))::cs) ->
      try(
        let subst_for_head = unifyAtom g head

        in let newGoalsInsideChosenClause = map (atomSubst subst_for_head) elems

        in let substProveNewGoals =  match newGoalsInsideChosenClause with
            | [] -> (fun x -> subst_for_head (s x))
            | (x::_) ->
              let id = genFunctionId x
              in let clauses = reverse (Hashtbl.find_all hashTable id)
              in  prove clauses hashTable newGoalsInsideChosenClause (fun x -> subst_for_head (s x))

        in let remainingGoals = (map (fun atom -> atomSubst (fun x -> substProveNewGoals(subst_for_head x)) atom  )) gs

        in match remainingGoals with
        | [] -> (fun x -> substProveNewGoals(subst_for_head (s x)))
        | (x::_) ->
          let id = genFunctionId x
          in let clauses = reverse (Hashtbl.find_all hashTable id)
          in  prove clauses hashTable remainingGoals (fun x -> substProveNewGoals(subst_for_head (s x)))

      )
      with False -> (prove cs hashTable goals s)


let interpret (Program(Sentence(clauses), Resolvant(goals))) =
  try (
    let hashT = genFunctionTable (Sentence(clauses))
    in let subst = match goals with
        | (g::_) ->
          let id = genFunctionId g
          in let clauses = reverse (Hashtbl.find_all hashT id)
          in  prove clauses hashT goals (fun x -> x)
        | [] -> fun x -> x
    in let res = map (fun g -> atomSubst subst g) goals
    in Ans(Resolvant(res))
  )
  with False -> Fail
