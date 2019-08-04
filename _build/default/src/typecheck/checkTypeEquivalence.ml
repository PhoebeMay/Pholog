open Core
open ParseTree
open Dt
open Logging

type t1 = (string * int) typeins list [@@deriving show]

type t2 = string typeins list [@@deriving show]

type in1 = (string * intnum) typeins [@@deriving show]

type in2 = string typeins [@@deriving show]

(* TODO DEDUP WITH OTHER VERSION? *)
let rec checkTypeEquiv x y =
  let () =
    logDebug (fun m -> m "check type equiv h %a and %a" pp_in1 x pp_in2 y)
  in
  match (x, y) with
  | _, TypeVar _ -> (*  TODO need to add to inferred map?? think  *)
                    ()
  | TypeVar ("Any", -1), _ -> ()
  | TypeVar (_name, _num), _notTypeVar ->
      raise (Err "TypeVar not equivalent to notTypeVar")
  | IntTyp, IntTyp -> ()
  | IntTyp, _ -> raise (Err "")
  | TypeCons (name, args), TypeCons (name2, args2) ->
      let () =
        logDebug (fun m ->
            m
              "check type equiv constructors :\n\
              \          \n\
              \ %a : %a  \n\n\n\n\
              \ %a : %a\n"
              pp_var name pp_t1 args pp_var name2 pp_t2 args2 )
      in
      if name = name2 && List.length args = List.length args2 then
        List.iter2_exn ~f:checkTypeEquiv args args2
      else
        raise
          (Err
             ( "Type constructor " ^ show_var name ^ " does not unify with "
             ^ show_var name2 ))
  | TypeCons (_, _), _ -> raise (Err "")

type a = (funct * intnum) typeins list [@@deriving show]

type b = var typeins list [@@deriving show]

let rec checkTypesAreEquivalent (list1 : (var * intnum) typeins sexp_list)
    (list2 : var typeins sexp_list) =
  let () =
    logDebug (fun m ->
        m "check types are equivalent list %a %a \n\n" pp_a list1 pp_b list2 )
  in
  match (list1, list2) with
  | x :: xs, y :: ys ->
      checkTypeEquiv x y ;
      checkTypesAreEquivalent xs ys
  | [], [] -> ()
  | _ ->
      raise
        (Err "type equalence fails because type lists are different in length")
