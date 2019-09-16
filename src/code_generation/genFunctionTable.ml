open Core
open ParseTree
open Dt
open Logging

(* Get location for variable *)
let makeVariAbstract ~varMap:mapping ~envCounter:envCounter
    ~tempCounter:tempCounter ~position:currentPosition ~useMap:lastuses
    ~isQuery:isQuery vari =
  let current = Hashtbl.find mapping vari
  in match current with
  | Some(x) -> x
  | None ->
    logDebug (fun m -> m "Make abstract %a " pp_var vari);
    let lastUse = Hashtbl.find_exn lastuses vari
    in let () = logDebug (fun m -> m "lastuse: %a, currentPosition: %a" pp_intnum lastUse pp_intnum currentPosition)
    in
    if ((lastUse <= 1 || currentPosition = lastUse) && not isQuery)
    then (
      let res = T(Temp(!tempCounter))
      in
      Hashtbl.add_exn mapping ~key:vari ~data:res;
      tempCounter := !tempCounter + 1;
      res
    )
    else (
      let res = E(Env(!envCounter))
      in
      Hashtbl.add_exn mapping ~key:vari ~data:res;
      envCounter := !envCounter + 1;
      res
    )



(* Env(setIfNotAlready mapping vari counter) *)

let rec makeTermAbstract
    ~varMap:mapping
    ~envCounter:counter
    ~useMap:lastuse
    ~position:currentPosition
    ~tempCounter:tempCounter
    ~isQuery:q term =
  match term with
  | TVar(var) -> TVar(makeVariAbstract
                        ~varMap:mapping
                        ~envCounter:counter
                        ~useMap:lastuse
                        ~position:currentPosition
                        ~tempCounter:tempCounter
                        ~isQuery:q var)
  | TFun(funtor, ts) ->
    TFun(funtor, List.map ~f:(makeTermAbstract
                                ~varMap:mapping
                                ~envCounter:counter
                                ~useMap:lastuse
                                ~position:currentPosition
                                ~tempCounter:tempCounter
                                ~isQuery:q) ts)
  | TInt(n) -> TInt(n)

let rec makeMathExprAbstract
    ~varMap:mapping
    ~envCounter:counter
    ~useMap:lastuse
    ~position:currentPosition
    ~tempCounter:tempCounter
    ~isQuery:q expr =
  match expr with
  | Subtract(e1,e2) ->
    Subtract(
      makeMathExprAbstract
        ~varMap:mapping
        ~envCounter:counter
        ~useMap:lastuse
        ~position:currentPosition
        ~tempCounter:tempCounter
        ~isQuery:q e1,  makeMathExprAbstract
        ~varMap:mapping
        ~envCounter:counter
        ~useMap:lastuse
        ~position:currentPosition
        ~tempCounter:tempCounter
        ~isQuery:q e2)
  | Plus(e1,e2) ->
    Plus(
      makeMathExprAbstract
        ~varMap:mapping
        ~envCounter:counter
        ~useMap:lastuse
        ~position:currentPosition
        ~tempCounter:tempCounter
        ~isQuery:q e1,
      makeMathExprAbstract
        ~varMap:mapping
        ~envCounter:counter
        ~useMap:lastuse
        ~position:currentPosition
        ~tempCounter:tempCounter
        ~isQuery:q e2)
  | Base(Int(n)) -> Base(Int(n))
  | Base(Var(v)) -> Base(Var(makeVariAbstract
                               ~varMap:mapping
                               ~useMap:lastuse
                               ~position:currentPosition
                               ~envCounter:counter
                               ~tempCounter:tempCounter
                               ~isQuery:q v))



let makeBodyValAbstract
    ~varMap:mapping
    ~envCounter:counter
    ~useMap:lastuse
    ~position:currentPosition
    ~tempCounter:tempCounter
    ~isQuery:q item =
  match item with
  | CAT(Atom(name,args)) ->
    CAT(Atom(name,
             List.map
               ~f:(makeTermAbstract
                     ~varMap:mapping
                     ~envCounter:counter
                     ~useMap:lastuse
                     ~position:currentPosition
                     ~tempCounter:tempCounter
                     ~isQuery:q)
               args
            ))
  | CAR(IsExpr(vari, mathexpr)) ->
    CAR(IsExpr(
        makeVariAbstract
          ~varMap:mapping
          ~envCounter:counter
          ~useMap:lastuse
          ~position:currentPosition
          ~tempCounter:tempCounter
          ~isQuery:q vari,makeMathExprAbstract
          ~varMap:mapping
          ~envCounter:counter
          ~useMap:lastuse
          ~position:currentPosition
          ~tempCounter:tempCounter
          ~isQuery:q mathexpr
      ))
  | Cut -> Cut
  | Fail -> Fail


let rec updateLastVariableUsePositionTerm result (currentPosition : int) term =
  logDebug (fun m -> m "updateLastVariableUsePositionTerm");
  match term with
  | TVar(var) ->
    let x = Hashtbl.change result var ~f:(fun _ -> Some(currentPosition))
    in x
  | TFun(_, ts) -> List.iter ts
                     ~f:(updateLastVariableUsePositionTerm result currentPosition)
  | TInt(_) -> ()

let rec updateLastVariableUsePositionMathExpr result currentPosition mathexpr =
  match mathexpr with
  |   Plus(x,y) ->
    updateLastVariableUsePositionMathExpr result currentPosition x;
    updateLastVariableUsePositionMathExpr result currentPosition y

  | Subtract(x,y) ->
    updateLastVariableUsePositionMathExpr result currentPosition x;
    updateLastVariableUsePositionMathExpr result currentPosition y

  | Base(Int(_)) -> ()
  | Base(Var(var)) ->
    let x = Hashtbl.change result var ~f:(fun _ -> Some(currentPosition))
    in x


let updateLastVariableUsePositionClauseBodyVal result currentPosition bv  =
  logDebug (fun m -> m "updateLastVariableUsePositionClauseBodyVal");
  match bv with
  | CAT(Atom(_name,args)) -> List.iter args
                              ~f:(updateLastVariableUsePositionTerm result currentPosition)

  | CAR(IsExpr(vari, mathexpr)) ->
    updateLastVariableUsePositionMathExpr result currentPosition mathexpr;
    Hashtbl.change result vari ~f:(fun _ -> Some(currentPosition))

  | Cut -> ()
  | Fail -> ()


let updateLastVariableUsePositionAtom result currentPosition (Atom(_, args))=
  List.iter args ~f:(updateLastVariableUsePositionTerm result currentPosition)

let rec getTermsLastUse result counter x =
  match x with
  | [] -> ()
  | (t::ts) ->
    logDebug (fun m -> m "hi");
    updateLastVariableUsePositionClauseBodyVal result counter t;
    getTermsLastUse result (counter + 1) ts

let getPositionLastVarUse (Clause(head, terms)) =
  let result = Hashtbl.create strKeyImp
  in let _ = updateLastVariableUsePositionAtom result 0 head
  in getTermsLastUse result 1 terms;
  result

let rec genAbstCBVList
    ~varMap:mapping
    ~envCounter:envVars
    ~useMap:lastuse
    ~position:num
    ~tempCounter:tempCounter
    ~isQuery:q bvs =
  match bvs with
  | [] -> []
  | t::ts -> let x = makeBodyValAbstract
                 ~varMap:mapping
                 ~envCounter:envVars
                 ~useMap:lastuse
                 ~position:num
                 ~tempCounter:tempCounter
                 ~isQuery:q t
    in x::(genAbstCBVList ts
             ~position:(num+1)
             ~varMap:mapping
             ~envCounter:envVars
             ~useMap:lastuse
             ~tempCounter:tempCounter
             ~isQuery:q)


let makeClauseAbstract c =
  match c with
  | C((Clause(Atom(name, args), terms))) ->
    let envVars = ref 0
    in let tempCounter = ref 0
    in let mapping = Hashtbl.create strKeyImp
    in let lastuse = getPositionLastVarUse (Clause(Atom(name, args), terms))

    in let () =
         (logDebug (fun m -> m "last use is"));
         Hashtbl.iteri
           lastuse
           ~f:(fun ~key:key ~data:value ->
               (logDebug (fun m -> m "(%a,%a)\n" pp_var key pp_intnum value)))

    in let abstHead =   List.map ~f:(makeTermAbstract
                                       ~varMap:mapping
                                       ~envCounter:envVars
                                       ~useMap:lastuse
                                       ~position:0
                                       ~tempCounter:tempCounter
                                       ~isQuery:false) args

    in let abstTail = genAbstCBVList
           ~varMap:mapping
           ~envCounter:envVars
           ~useMap:lastuse
           ~position:1
           ~tempCounter:tempCounter
           ~isQuery:false terms
    in let ansClause = (Clause(Atom(name,abstHead),abstTail))
    in let () = logDebug (fun m -> m "Abstract clause is %a" (pp_clause pp_location) ansClause)
    in (ansClause, !envVars)
  | _ -> raise Oops


let genFunctionTable ((Program(Sentence(prog),Resolvant(goals))) ) =
  let cls = List.filter
      ~f:(fun x -> match x with C(_) -> true | _ -> false )
      prog
  in let rec genFunctionTableH clauses (hashT)  =
       match clauses with
       | [] -> hashT
       | ((Clause(Atom(name,args), body),n)::cs) ->
         Hashtbl.add_multi hashT
           ~key:(AbstractF(name, List.length args))
           ~data:(Clause(Atom(name,args), body),n);
         genFunctionTableH cs hashT

  in
  let table = genFunctionTableH (List.map ~f:makeClauseAbstract cls)
      (Hashtbl.create functionTKeyImp)
  in let () = logDebug (fun m -> m "Finish with clauses body")
  in let mapping = Hashtbl.create strKeyImp
  in let lastuse =  (Hashtbl.create strKeyImp)
  in let () = getTermsLastUse lastuse 0 goals
  in let counter = ref 0
  in let tempCounter = ref 0
  (* in let lastuse = List.iter ~f:(genAbstCBVList ~varMap:mapping ~envCounter:counter ) goals *)
  (* in let query = List.map ~f:(makeBodyValAbstract ~varMap:mapping ~envCounter:counter) goals *)
  (* in let lastuse = getPositionLastVarUse (Clause(Atom(name, args), terms)) *)
  (* in let abstHead =   List.map ~f:(makeTermAbstract ~varMap:mapping ~envCounter:envVars ~useMap:lastuse ~position:0 ~tempCounter:tempCounter) args *)
  (* in let rec genAbstTail bvs num =
       match bvs with
       | [] -> []
       | t::ts -> let x = makeBodyValAbstract ~varMap:mapping ~envCounter:envVars ~useMap:lastuse ~position:num ~tempCounter:tempCounter t
         in x::(genAbstTail ts (num+1)) *)
  in let query = genAbstCBVList ~varMap:mapping ~envCounter:counter ~useMap:lastuse  ~position:1 ~tempCounter:tempCounter ~isQuery:true goals
  in table, (query, !counter)
