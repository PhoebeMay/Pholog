open Core
open Dt
open ParseTree
open Logging

let rec unifyTypesOfInferredVars result a b =
  let () =
    logDebug (fun m ->
        m "unifyTypesOfInferredVars %a and %a\n\n" pp_infv a pp_infv b)
  in
  match (a, b) with
  | TypeVar (x, 0), TypeVar (y, 0) ->
      if x = y then ()
      else raise (Err ("Different user-defined types do not unify " ^ x ^ y))
  | TypeVar (x, 0), TypeVar (y, n) -> (
      logDebug (fun m -> m "hiiii");
      match Hashtbl.find result (y, n) with
      | None ->
          if (y, n) = (x, 0) then ()
          else Hashtbl.add_exn result ~key:(y, n) ~data:(TypeVar (x, 0))
      | Some res -> unifyTypesOfInferredVars result a res )
  | TypeVar (y, n), TypeVar (x, 0) -> (
      match Hashtbl.find result (y, n) with
      | None -> Hashtbl.add_exn result ~key:(y, n) ~data:(TypeVar (x, 0))
      | Some res -> unifyTypesOfInferredVars result res b )
  | TypeVar (y, n), TypeVar (x, n2) -> (
      if (y, n) = (x, n2) then ()
      else if n < n2 then
        match Hashtbl.find result (y, n) with
        | None -> Hashtbl.add_exn result ~key:(y, n) ~data:(TypeVar (x, n2))
        | Some res -> unifyTypesOfInferredVars result res b
      else
        match Hashtbl.find result (x, n2) with
        | None -> Hashtbl.add_exn result ~key:(x, n2) ~data:(TypeVar (y, n))
        | Some res -> unifyTypesOfInferredVars result a res )
  | TypeVar x, other -> (
      match Hashtbl.find result x with
      | None -> Hashtbl.add_exn result ~key:x ~data:other
      | Some res -> unifyTypesOfInferredVars result res other )
  | other, TypeVar x -> (
      match Hashtbl.find result x with
      | None -> Hashtbl.add_exn result ~key:x ~data:other
      | Some res -> unifyTypesOfInferredVars result other res )
  | TypeCons (n1, ag1), TypeCons (n2, ag2) ->
      if n1 = n2 then
        List.iter2_exn ~f:(unifyTypesOfInferredVars result) ag1 ag2
      else raise (Err "types no unify")
  | IntTyp, IntTyp -> ()
  | IntTyp, _ -> raise (Err "types no unify")
  | _, IntTyp -> raise (Err "types no unify")

let rec unifyTypesDecVarToRenamed predVariableBindings bodyVariableBindings a b
    =
  let () =
    logDebug (fun m ->
        m "unifyTypesDecVarToRenamed %a  and %a\n\n" (pp_typeins pp_var) a
          pp_infv b)
  in
  match (a, b) with
  | TypeVar x, y -> (
      match Hashtbl.find predVariableBindings x with
      | None -> Hashtbl.add_exn predVariableBindings ~key:x ~data:y
      | Some res -> unifyTypesOfInferredVars bodyVariableBindings res y )
  | TypeCons (n1, ag1), TypeCons (n2, ag2) ->
      if n1 = n2 then
        List.iter2_exn
          ~f:
            (unifyTypesDecVarToRenamed predVariableBindings
               bodyVariableBindings)
          ag1 ag2
      else raise (Err "types no unify")
  | TypeCons (_, _), TypeVar ("Any", -1) -> ()
  | TypeCons (_, _), TypeVar (_, _) -> raise (Err "Case should not occur")
  | IntTyp, IntTyp -> ()
  | IntTyp, _ -> raise (Err "types no unify")
  | _, IntTyp -> raise (Err "types no unify")

let unifyTypesList xs ys =
  let result = Hashtbl.create strIntKeyImp in
  let () = List.iter2_exn ~f:(unifyTypesOfInferredVars result) xs ys in
  result

let unifyTypesDecVarToRenamedList xs ys =
  let predBindings = Hashtbl.create strKeyImp in
  let varBindings = Hashtbl.create strIntKeyImp in
  let () =
    List.iter2_exn
      ~f:(unifyTypesDecVarToRenamed predBindings varBindings)
      xs ys
  in
  (predBindings, varBindings)

let rec appVarSubst varBindings x =
  match x with
  | TypeVar y -> (
      match Hashtbl.find varBindings y with
      | Some y -> appVarSubst varBindings y
      | None -> x )
  | TypeCons (name, args) ->
      TypeCons (name, List.map ~f:(appVarSubst varBindings) args)
  | IntTyp -> IntTyp

let rec appDefSubst
    (predBindings : (var, (var * intnum) typeins) Base.Hashtbl.t) varBindings x
    =
  match Hashtbl.find predBindings x with
  | Some y -> (
      match y with
      | TypeVar x -> appVarSubst varBindings (TypeVar x)
      | other -> other )
  | None -> TypeVar ("Any", -1)

let applySubst ls predBindings varBindings =
  List.map ~f:(appDefSubst predBindings varBindings) ls

let rec appSubstTypeinst subst typ =
  match typ with
  | TypeVar x -> (
      let y = Hashtbl.find subst x in
      match y with Some e -> e | None -> TypeVar x )
  | IntTyp -> IntTyp
  | TypeCons (n, args) ->
      TypeCons (n, List.map ~f:(appSubstTypeinst subst) args)

type tres = (string * int) typeins list [@@deriving show]

type typlis = string typeins list [@@deriving show]

type typlisn = (string * int) typeins list [@@deriving show]

type strint = string * int [@@deriving show]

(* TODO is this the same as the unify types method *)
let rec checkSubTypeBound varTypeMapping x y =
  match (x, y) with
  | TypeVar (n1, 0), TypeVar (n2, 0) ->
      if n1 = n2 then () else raise (Err "No sub type user defined var")
  | TypeVar (x, 0), TypeVar (y, n) -> (
      match Hashtbl.find varTypeMapping (y, n) with
      | None ->
          Hashtbl.add_exn varTypeMapping ~key:(y, n) ~data:(TypeVar (x, 0))
      | Some res -> checkSubTypeBound varTypeMapping (TypeVar (x, 0)) res )
  | TypeVar (y, n), TypeVar (x, 0) -> (
      match Hashtbl.find varTypeMapping (y, n) with
      | None ->
          Hashtbl.add_exn varTypeMapping ~key:(y, n) ~data:(TypeVar (x, 0))
      | Some res -> checkSubTypeBound varTypeMapping res (TypeVar (x, 0)) )
  | TypeVar (y, n), TypeVar (x, n2) -> (
      if (y, n) = (x, n2) then ()
      else if n < n2 then
        match Hashtbl.find varTypeMapping (y, n) with
        | None ->
            Hashtbl.add_exn varTypeMapping ~key:(y, n) ~data:(TypeVar (x, n2))
        | Some res -> checkSubTypeBound varTypeMapping res (TypeVar (x, n2))
      else
        match Hashtbl.find varTypeMapping (x, n2) with
        | None ->
            Hashtbl.add_exn varTypeMapping ~key:(x, n2) ~data:(TypeVar (y, n))
        | Some res -> checkSubTypeBound varTypeMapping (TypeVar (y, n)) res )
  | _ -> raise Oops

let rec checkSubType predTypeMapping varTypeMapping x y =
  let () =
    logDebug (fun m ->
        m "check is sub type %a %a \n\n" (pp_typeins pp_var) x
          (pp_typeins pp_strint) y)
  in
  match (x, y) with
  | TypeVar x, TypeVar (n, 0) -> (
      match Hashtbl.find predTypeMapping x with
      | None -> Hashtbl.add_exn predTypeMapping ~key:x ~data:(TypeVar (n, 0))
      | Some e ->
          if e = TypeVar (n, 0) then ()
          else logDebug (fun m -> m "found %a" pp_infv e);
          unifyTypesOfInferredVars varTypeMapping e (TypeVar (n, 0))
      (* raise (Err "Will not unify type constrained by annotation") *) )
  | TypeVar x, TypeVar (name, num) -> (
      match Hashtbl.find predTypeMapping x with
      | None ->
          Hashtbl.add_exn predTypeMapping ~key:x ~data:(TypeVar (name, num))
      | Some e ->
          unifyTypesOfInferredVars varTypeMapping e (TypeVar (name, num)) )
  | TypeVar x, other -> (
      match Hashtbl.find predTypeMapping x with
      | None -> Hashtbl.add_exn predTypeMapping ~key:x ~data:other
      | Some e -> unifyTypesOfInferredVars varTypeMapping e other )
  | _, TypeVar (_, 0) ->
      raise (Err "Will not unify type constrained by annotation")
  | _, TypeVar ("Any", -1) ->
      (* TODO try to break this case  *)
      ()
  (* TODO TYPEVAR N CASE *)
  | IntTyp, IntTyp -> ()
  | IntTyp, _ -> raise (Err "")
  | TypeCons (name, args), TypeCons (name2, args2) ->
      if name = name2 then
        List.iter2_exn
          ~f:(checkSubType predTypeMapping varTypeMapping)
          args args2
      else
        raise
          (Err
             ( "Type constructor " ^ show_var name ^ " does not unify with "
             ^ show_var name2 ))
  | TypeCons (_, _), _ -> raise (Err "")

let rec checkIsSubType predTypeMapping varTypeMapping
    (list1 : string typeins sexp_list)
    (list2 : (string * intnum) typeins sexp_list) =
  let () =
    logDebug (fun m ->
        m "check is sub type list %a %a \n\n" pp_typlis list1 pp_typlisn list2)
  in
  match (list1, list2) with
  | x :: xs, y :: ys ->
      checkSubType predTypeMapping varTypeMapping x y;
      checkIsSubType predTypeMapping varTypeMapping xs ys
  | [], [] -> ()
  | _ -> raise (Err "Type lists have different lengths so aren't equivalent")
