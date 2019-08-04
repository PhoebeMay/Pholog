open Core
open Dt
open Logging
open Initialise
open RuntimeDataStructures
open StackImplementation

let cyclic x y =
  match x with
  | B x -> (
    match y with
    | B yp -> if Core.phys_equal yp x then true else false
    | _ -> false )
  | _ -> raise Oops

let bind z y trail =
  match z with
  | B x ->
      if not (cyclic (B x) y) then
        let () = x := y in
        createNew (Var x) ~prev:trail
      else trail
  | _ -> raise Oops

let rec rewindTrail trail =
  logDebug (fun m -> m "backtrack from rewind trail") ;
  match trail with
  | Some node -> (
    match node.value with
    | Label ->
        (* Done *)
        logDebug (fun m -> m "finished backtracking") ;
        node.prev
    | Var hv ->
        logDebug (fun m -> m "unbinding from %a" pp_trailVal (Var hv)) ;
        hv := UnboundVar ;
        rewindTrail node.prev )
  | None -> raise (Err "trail empty on rewind")

let rewindStack (c : choicePoint node) compState = c.value.stack

let backtrack compState =
  logDebug (fun m -> m "backtrack") ;
  match compState.choicePoints with
  | None -> raise Fail
  | Some c ->
      let newTrail =
        rewindTrail compState.trail
        (* TODO CHECK *)
        (* in let _ = match (c.value.stack) with
        | (Some(e : environment node)) ->
          compState.returnCps <- e.value.callerCps
    | None -> compState.returnCps <- None *)
      in
      (* Do i need to update nextOptionPointer somewhere? *)
      compState.cp <- c.value.nextOptionPointer ;
      compState.trail <- newTrail ;
      (* compState.returnTrailPoint <- newTrail; *)
      compState.arguments <- Array.copy c.value.arguments ;
      compState.returnAddress <- c.value.returnAddr ;
      compState.envStack <- rewindStack c compState ;
      compState.returnCps <- c.value.returnCps ;
      compState.returnTrailPoint <- c.value.returnTrailPoint ;
      if
        List.exists (getPreds compState.returnCps) ~f:(fun x ->
            Some x = compState.choicePoints )
      then raise Oops
      else () ;
      compState

let rec unifyLists xs ys compState =
  logDebug (fun m -> m "unify lists") ;
  let rec unifyListsH xs ys counter compState =
    if counter >= Array.length xs then (
      logDebug (fun m -> m "unify lists done") ;
      compState )
    else
      (* TODO careful *)
      let cp = compState.cp in
      let newCompState = unifyH xs.(counter) ys.(counter) compState in
      if newCompState.cp = cp then unifyListsH xs ys (counter + 1) newCompState
      else compState
  in
  unifyListsH xs ys 0 compState

and unifyH ah bh compState =
  let () =
    logDebug (fun m -> m "unifyH %a %a" pp_heapValue ah pp_heapValue bh)
  in
  match (ah, bh) with
  | InitH, _ -> raise Oops
  | _, InitH -> raise Oops
  | B x, B y -> (
      logDebug (fun m -> m "DOUBLE BOUND UNIF") ;
      match (!x, !y) with
      | UnboundVar, UnboundVar ->
          compState.trail <- bind (B x) (B y) compState.trail ;
          compState
      | UnboundVar, B z -> (
        match !z with
        | UnboundVar ->
            compState.trail <- bind (B x) (B z) compState.trail ;
            compState
        | _ -> unifyH (B x) !z compState )
      | UnboundVar, a ->
          compState.trail <- bind (B x) a compState.trail ;
          compState
      | B z, UnboundVar -> (
        match !z with
        | UnboundVar ->
            compState.trail <- bind (B y) (B z) compState.trail ;
            compState
        | _ -> unifyH (B y) !z compState )
      | a, UnboundVar ->
          compState.trail <- bind (B y) a compState.trail ;
          compState
      | _, _ -> unifyH !x !y compState )
  | B x, y -> tryBind x y compState
  | y, B x -> tryBind x y compState
  | StrPointer (x, xs), StrPointer (y, ys) ->
      if x = y then unifyLists xs ys compState else backtrack compState
  | UnboundVar, _ -> raise Oops
  | _, UnboundVar -> raise Oops
  | Int n1, Int n2 -> if n1 = n2 then compState else backtrack compState
  | x, y ->
      logError (fun m ->
          m "UnifyH failed: %a %a \n" pp_heapValue x pp_heapValue y ) ;
      backtrack compState

and tryBind x y compState =
  if !x = UnboundVar then
    if not (cyclic (B x) y) then (
      let newTrail = createNew (Var x) ~prev:compState.trail in
      x := y ;
      compState.trail <- newTrail ;
      compState )
    else compState
  else unifyH !x y compState

let unifyS a b compState =
  (* logDebug (fun m -> m "unify"); *)
  match (a, b) with
  | InitV, _ -> raise (Err "uninitialised")
  | _, InitV -> raise Oops
  | HeapPointer ah, HeapPointer bh -> unifyH ah bh compState
  | _, _ -> raise Oops
