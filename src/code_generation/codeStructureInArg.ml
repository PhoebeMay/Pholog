open Core
open Dt
open ParseTree
open Logging
open Utils

type abstractInstruction1 =
  | Abs_UnifyE of location
  | Abs_StructGetVariableT of temp
  | Abs_GetStructureT of structure * temp
  | Abs_StructGetInt of int

type getArg = {
  unify : abstractInstruction1;
  body : abstractInstruction1 list;
}

let getStructMapInt (structMapGen : structMapGen) str =
  let x = Hashtbl.find structMapGen.mapping str in
  match x with
  | Some n -> n
  | None ->
      let next = structMapGen.nextFree in
      Hashtbl.add_exn structMapGen.mapping ~key:str ~data:next;
      structMapGen.nextFree <- next + 1;
      next

let rec codeArgStructAbs nextFreeTemp (term : location term)
    (structMapGen : structMapGen) =
  match term with
  | TVar var ->
      ( (* logDebug (fun m -> m "codeArgStruct %a" (pp_term pp_env) term ); *)
        { unify = Abs_UnifyE var; body = [] }
        : getArg )
  | TFun (name, ts) ->
      let arity = List.length ts in
      let nextFree = Temp !nextFreeTemp in
      let _ = nextFreeTemp := !nextFreeTemp + 1 in
      {
        unify = Abs_StructGetVariableT nextFree;
        body =
          Abs_GetStructureT
            ((getStructMapInt structMapGen name, arity), nextFree)
          :: codeArgStructsAbs ts nextFreeTemp structMapGen;
      }
  | TInt n -> ({ unify = Abs_StructGetInt n; body = [] } : getArg)

and codeArgStructsAbs terms (nextFreeTemp : int ref) structMapGen =
  let rec codeArgStructsH terms (nextFreeTemp : int ref) structMapGen =
    match terms with
    | t :: ts ->
        (* let () = logDebug(fun m -> m "codeArgStructsH for term %a" (pp_term pp_env) t) *)
        (* in  *)
        let res = codeArgStructAbs nextFreeTemp t structMapGen in
        res :: codeArgStructsH ts nextFreeTemp structMapGen
    | [] -> []
  in
  let collect results =
    let collectHead results =
      List.map ~f:(fun { unify = h; body = _ } -> h) results
    in
    let collectTail results =
      List.map ~f:(fun { unify = _; body = t } -> t) results
    in
    let head = collectHead results in
    let tail = List.concat (collectTail results) in
    head @ tail
  in
  collect (codeArgStructsH terms nextFreeTemp (structMapGen : structMapGen))

let removeAbstractInstrFun seenSoFar x =
  match x with
  | Abs_UnifyE var ->
      if seen var seenSoFar then StructGetValue var else StructGetVariable var
  | Abs_GetStructureT (x, y) -> GetStructureT (x, y)
  | Abs_StructGetVariableT t -> StructGetVariable (T t)
  | Abs_StructGetInt n -> StructGetInt n

let removeAbstractInstr xs seenSoFar =
  List.map ~f:(removeAbstractInstrFun seenSoFar) xs

let codeArgStructs xs nextFreeTemp seenSoFar structMapGen =
  removeAbstractInstr
    (codeArgStructsAbs xs nextFreeTemp structMapGen)
    seenSoFar
