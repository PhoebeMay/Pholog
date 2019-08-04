open ParseTree

exception UnifiyFail
exception False

let rec length xs =
  match xs with
    (_::ys) -> 1 + length ys
  | [] -> 0

let rec mapTerms f xs =
  match xs with
  | (y::ys) ->
    let res = Printf.printf "Before %s \n" (strTerm y);
      f y
    in Printf.printf "After %s \n" (strTerm res);
    res :: mapTerms f ys
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
    Printf.printf "Create subst unifyTerm 1: [%s/%s]\n" (strTerm t2) (strTerm t1);
    fun x ->
      if x = t1
      then
        (Printf.printf "match\n"; t2)
      else
        (Printf.printf "no match\n";  x)
  | (_,TVar(_)) ->
    Printf.printf "Create subst unifyTerm 2\n";
    fun x -> if x = t2 then t1 else x
  | (TFun(funct1, ts1), TFun(funct2, ts2)) ->
    Printf.printf "Create subst unifyTerm 3\n";
    if funct1 = funct2
    then unifyTermLists ts1 ts2
    else (raise False)

let unifyAtom (Atom(p1, ts1)) (Atom(p2, ts2))  =
  if (length ts1) <> (length ts2)
  then raise False
  else if p1 <> p2
  then raise False
  else unifyTermLists ts1 ts2

let atomSubst subst (Atom(p1, ts1)) =
  let res =
    Printf.printf "Atom subst %s \n" (strAtom (Atom(p1, ts1)));
    Atom(p1, mapTerms subst ts1)
  in Printf.printf "Atom subst gives %s \n" (strAtom res);
  res

    (*
let rec proveBody subst body allClauses =
  match body with
  | [] -> subst
  | (b::bs) ->
    let new_subst = prove allClauses allClauses (atomSubst subst b) in
    proveBody (fun x -> new_subst (subst x)) bs allClauses
*)

let rec prove clauses allClauses goals s =
  match goals with
    [] -> s
  | (goal::gs) ->
    match clauses with
      (Clause(head, elems)::cs) ->
      (
        try (let subst_for_head =
               Printf.printf "The goal is: %s \n" (strAtom goal);
               Printf.printf "The clause is: %s\n\n" (strClause (Clause(head, elems)));
               unifyAtom goal head
             in let newGoalsInsideChosenClause = map (atomSubst subst_for_head) elems
             in let subst_for_newgoals = prove allClauses allClauses newGoalsInsideChosenClause (fun x -> subst_for_head (s x))
             in let newSetOfAllGoals = ( map (fun atom -> atomSubst (fun x -> subst_for_newgoals(subst_for_head x)) atom  ) ) gs
             in let ans = prove allClauses allClauses newSetOfAllGoals (fun x -> subst_for_newgoals(subst_for_head (s (x))))
             in
             Printf.printf "The new goals are: %s\n" (strAll strAtom newGoalsInsideChosenClause);
             ans
            )
        with False -> (
            Printf.printf "backtrack from goal:%s\n clause: %s\n\n" (strAtom goal) (strClause (Clause(head, elems)));
            prove cs allClauses (goal::gs) s)
      )
    | [] -> Printf.printf "out of clauses\n"; raise False

let interpret (Program(Sentence(clauses), Resolvant(goals))) =
  Printf.printf "Interpreter 1";
  (try
     (Printf.printf "%s\n\n" (strProgram (Program(Sentence(clauses), Resolvant(goals))));
      let subst = prove clauses clauses goals (fun x->x)
      in let res = map (fun g -> atomSubst subst g) goals
      in Ans(Resolvant(res))
     )
   with False -> Fail
  )
