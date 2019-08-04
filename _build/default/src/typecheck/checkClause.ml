open Core
open Logging
open Dt
open ParseTree
open UnifyTypes

let rec getTypeOfArgumentTerm variableTypes
    (constructorToType :
      (string * int, string * string list * string typeins list) Hashtbl.t) x =
  match x with
  | TVar x -> (
      let varType = Hashtbl.find variableTypes x in
      match varType with
      | Some e -> e
      | None ->
          logError (fun m -> m "lokoup failed for %a" pp_var x) ;
          raise Oops )
  | TInt _ -> IntTyp
  | TFun (name, funargs) ->
      let () =
        logDebug (fun m -> m "getargtypelist %a" pp_var name)
        (* Get relevavant type declaration *)
      in
      let typ, tvars, body =
        Hashtbl.find_exn constructorToType (name, List.length funargs)
      in
      let tvars_typ =
        List.map
          ~f:(getTypeOfArgumentTerm variableTypes constructorToType)
          funargs
      in
      let predBindings, varBindings =
        unifyTypesDecVarToRenamedList body tvars_typ
      in
      let tres = applySubst tvars predBindings varBindings in
      let () = logDebug (fun m -> m "tres is %a" pp_tres tres) in
      let () =
        logDebug (fun m ->
            m "gettypeofargumenttermres  %a %a" pp_var name pp_infv
              (TypeCons (typ, tres)) )
      in
      TypeCons (typ, tres)

let getPredicateArgumentTypes args variableTypes constructorToType =
  List.map ~f:(getTypeOfArgumentTerm variableTypes constructorToType) args

let typeCheckAtom predTypes variableTypes constructorToType binding
    (Atom (pred, terms)) =
  let predType = Hashtbl.find_exn predTypes pred in
  let argTypeList =
    getPredicateArgumentTypes terms variableTypes constructorToType
    (* in let () = checkIsSubType (Hashtbl.create strKeyImp) (Hashtbl.create strIntKeyImp) predType argTypeList *)
  in
  let () =
    checkIsSubType (Hashtbl.create strKeyImp) binding predType argTypeList
  in
  ()

let typeCheckMathExpr _body _variableTypes =
  (* TODO shouldn't need this function because all ints detected during variable types gen scan  *)
  ()

let typeCheckIs (IsExpr (x, body)) variableTypes =
  match Hashtbl.find variableTypes x with
  | Some IntTyp -> typeCheckMathExpr body variableTypes
  | _ -> raise (Err "Not int in arith expr")

let typeCheckCBV predTypes variableTypes constructorToType binding cbv =
  match cbv with
  | CAT a -> typeCheckAtom predTypes variableTypes constructorToType binding a
  | Fail -> ()
  | Cut -> ()
  | CAR (IsExpr (x, body)) -> typeCheckIs (IsExpr (x, body)) variableTypes
