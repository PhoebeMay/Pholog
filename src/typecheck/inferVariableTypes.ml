open Core
open Dt
open Logging
open ParseTree
open UnifyTypes

let rec newTVar nextFreeType typ =
  match typ with
  | TypeVar _ ->
      nextFreeType := !nextFreeType + 1;
      TypeVar ("Autogen", !nextFreeType - 1)
  | IntTyp -> IntTyp
  | TypeCons (name, args) ->
      TypeCons (name, List.map ~f:(newTVar nextFreeType) args)

let rec argTVar typ =
  match typ with
  | TypeVar name -> TypeVar (name, 0)
  | IntTyp -> IntTyp
  | TypeCons (name, args) -> TypeCons (name, List.map ~f:argTVar args)

let rec getTypesOfVariablesFunctorBody name args constructorToType nextFreeType
    result =
  let () = logDebug (fun m -> m "getTypesOfVariablesFunctor %a" pp_var name) in
  let _, _, cases =
    Hashtbl.find_exn constructorToType (name, List.length args)
  in
  let rec iterArgs xs counter =
    match xs with
    | [] -> ()
    | TInt _ :: ags -> iterArgs ags (counter + 1)
    | TFun (name, args) :: ags ->
        (* TODO do i need to propegate type informaiton into the recursive call? *)
        let () =
          getTypesOfVariablesFunctorBody name args constructorToType
            nextFreeType result
        in
        iterArgs ags (counter + 1)
    | TVar var :: ags ->
        let typ = newTVar nextFreeType (List.nth_exn cases counter) in
        let () =
          Hashtbl.update result var ~f:(fun prev ->
              match prev with Some e -> typ :: e | None -> [ typ ])
        in
        iterArgs ags (counter + 1)
  in
  iterArgs args 0

let getTypesOfVariablesAtom predTypes constructorToType result nextFreeType
    (Atom (name, args)) =
  let () = logDebug (fun m -> m "getTypesOfVariablesAtom %a" pp_var name) in
  let argType = Hashtbl.find_exn predTypes name in
  let rec iterArgs xs counter =
    match xs with
    | [] -> ()
    | TInt _ :: ags -> iterArgs ags (counter + 1)
    | TFun (name, args) :: ags ->
        (* let expectedTyp = List.nth_exn argType counter in  *)
        let () =
          getTypesOfVariablesFunctorBody name args constructorToType
            nextFreeType result
        in
        iterArgs ags (counter + 1)
    | TVar var :: ags ->
        let expectedTyp = List.nth_exn argType counter in
        let typ = newTVar nextFreeType expectedTyp in
        let () =
          Hashtbl.update result var ~f:(fun prev ->
              match prev with Some e -> typ :: e | None -> [ typ ])
        in
        iterArgs ags (counter + 1)
  in
  iterArgs args 0

let rec genMapping targs expectedargs result =
  match (targs, expectedargs) with
  | [], [] -> ()
  | x :: xs, y :: ys ->
      Hashtbl.add_exn result ~key:x ~data:y;
      genMapping xs ys result
  | _, _ -> raise Oops

let rec instantiate mapping typ =
  match typ with
  | IntTyp -> IntTyp
  | TypeVar name -> Hashtbl.find_exn mapping name
  | TypeCons (name, args) ->
      TypeCons (name, List.map ~f:(instantiate mapping) args)

let rec instantiateTDR mapping (TypeDefRight (name, args)) =
  TypeDefRight (name, List.map ~f:(instantiate mapping) args)

let rec getTypesOfVariablesFromExpectedType typeDefs expectedType result term =
  match term with
  | TInt _ -> (
      match expectedType with
      | IntTyp -> ()
      | _ -> raise (Err "Type mismatch saw int") )
  | TFun (structName, args) -> (
      match expectedType with
      | TypeCons (typeName, expectedargs) ->
          let targs, cases = Hashtbl.find_exn typeDefs typeName in
          let subst = Hashtbl.create strKeyImp in
          let () = genMapping targs expectedargs subst in
          let () = logDebug (fun m -> m "lookup %a" pp_var structName) in
          let thisCase =
            List.find_exn cases ~f:(fun (TypeDefRight (defname, _)) ->
                defname = structName)
          in
          let (TypeDefRight (n, rcases)) = instantiateTDR subst thisCase in
          List.iter2_exn
            ~f:(fun a b ->
              getTypesOfVariablesFromExpectedType typeDefs a result b)
            rcases args
      | _ ->
          raise
            (Err
               ( "Type mismatch expected type "
               ^ show_typeins pp_var expectedType
               ^ " saw struct " ^ structName )) )
  | TVar var ->
      let () = logDebug (fun m -> m "Add type of var %a" pp_var var) in
      let typ = argTVar expectedType in
      let () =
        Hashtbl.update result var ~f:(fun prev ->
            match prev with Some e -> typ :: e | None -> [ typ ])
      in
      ()

let getTypesOfVariablesClauseHead
    (predTypes : (var, var typeins sexp_list) Base.Hashtbl.t) typeDefs result
    (Atom (name, args)) =
  let argType = Hashtbl.find_exn predTypes name in
  let rec iterArgs xs counter =
    match xs with
    | [] -> ()
    | term :: ys ->
        let expectedType = List.nth_exn argType counter in
        getTypesOfVariablesFromExpectedType typeDefs expectedType result term;
        iterArgs ys (counter + 1)
  in
  iterArgs args 0

let rec getTypesFromMathExpr result mexpr =
  match mexpr with
  | Plus (a, b) ->
      getTypesFromMathExpr result a;
      getTypesFromMathExpr result b
  | Subtract (a, b) ->
      getTypesFromMathExpr result a;
      getTypesFromMathExpr result b
  | Base (Var var) ->
      Hashtbl.update result var ~f:(fun prev ->
          match prev with Some e -> IntTyp :: e | None -> [ IntTyp ])
  | Base (Int _) -> ()

let getTypesFromIs result (IsExpr (var, mathExpr)) =
  let () =
    Hashtbl.update result var ~f:(fun prev ->
        match prev with Some e -> IntTyp :: e | None -> [ IntTyp ])
  in
  getTypesFromMathExpr result mathExpr

let getTypesOfVariablesCBV predTypes consToTyp result nextFreeType cbv =
  match cbv with
  | CAT x -> getTypesOfVariablesAtom predTypes consToTyp result nextFreeType x
  | CAR x -> getTypesFromIs result x
  | Cut -> ()
  | Fail -> ()

let getMGUVariableType
    (variableTypes : (var, (var * intnum) typeins sexp_list) Base.Hashtbl.t)
    variable =
  (* let () = logDebug (fun m -> m "getMGUVariableType %a" pp_var variable ) *)
  let allTypes = Hashtbl.find_exn variableTypes variable in
  let first = List.nth_exn allTypes 0 in
  let s = Hashtbl.create strIntKeyImp in
  let () = List.iter ~f:(unifyTypesOfInferredVars s first) allTypes in
  let result =
    appSubstTypeinst s first
    (* in let () = logDebug (fun m -> m "ans is %a" (pp_typeins pp_var) result ) *)
  in
  result

type myrhs = (string * int) typeins [@@deriving show]

let getTypesOfVariablesQuery query predTypes consToTyp =
  let varToUses = Hashtbl.create strKeyImp in
  let nextFreeType = ref 1 in
  let () =
    List.iter
      ~f:(getTypesOfVariablesCBV predTypes consToTyp varToUses nextFreeType)
      query
  in
  let result = Hashtbl.create strKeyImp in
  let () =
    Hashtbl.iteri
      ~f:(fun ~key:k ~data:_ ->
        Hashtbl.add_exn result ~key:k ~data:(getMGUVariableType varToUses k))
      varToUses
  in
  let () = logDebug (fun m -> m "Variable types map is ") in
  let () =
    Hashtbl.iteri
      ~f:(fun ~key:k ~data:v ->
        logDebug (fun m -> m "%a, %a" pp_var k pp_myrhs v))
      result
  in
  result

type d = (funct * intnum) typeins list [@@deriving show]

type varlis = var typeins list [@@deriving show]

let getTypesOfVariables (Clause (head, body)) predTypes typeDefs consToTyp =
  let varToUses = Hashtbl.create strKeyImp in
  let () = getTypesOfVariablesClauseHead predTypes typeDefs varToUses head in
  let nextvar = ref 1 in
  let () =
    List.iter
      ~f:(getTypesOfVariablesCBV predTypes consToTyp varToUses nextvar)
      body
  in
  let () = logDebug (fun m -> m "var to uses is ") in
  let () =
    Hashtbl.iteri
      ~f:(fun ~key:k ~data:d -> logDebug (fun m -> m "%a %a" pp_var k pp_d d))
      varToUses
  in
  let result = Hashtbl.create strKeyImp in
  let toiter ~key:k ~data:_ =
    let dataToAdd = getMGUVariableType varToUses k in
    Hashtbl.add_exn result ~key:k ~data:dataToAdd
  in
  let () = Hashtbl.iteri ~f:toiter varToUses in
  let () = logDebug (fun m -> m "\n Variable types map is ") in
  let () =
    Hashtbl.iteri
      ~f:(fun ~key:k ~data:v ->
        logDebug (fun m -> m "%a, %a" pp_var k pp_myrhs v))
      result
  in
  let () = logDebug (fun m -> m "") in
  result
