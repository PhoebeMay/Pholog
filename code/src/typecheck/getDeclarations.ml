open Core
open Dt
open ParseTree
open Logging


let getPredTypes body =
  let result = Hashtbl.create strKeyImp
  in let () = List.iter
         ~f:(fun x ->
             match x with
               (P(PredDef(name,types))) ->  Hashtbl.add_exn result ~key:name ~data:types
             | _ -> ()
           )
         body
  in result



type 'a typdefright = funct list * 'a typeDefRight list [@@deriving show]

let getTypeDefs body =
  let result = Hashtbl.create strKeyImp
  in let () = List.iter
         ~f:(fun x ->
             match x with
               (D(TypeDef(name,vars,cases))) -> Hashtbl.add_exn result ~key:name ~data:(vars,cases)
             | _ -> ()
           )
         body
  in let () = logDebug (fun m -> m "The type defs are")
  in let () = Hashtbl.iteri
         ~f:(fun ~key:k ~data:v -> logDebug (fun m -> m "%a, %a" pp_funct k (pp_typdefright pp_var) v))
         result
  in let () = logDebug (fun m -> m "")
  in result





  let reverse (typeDefs : (var, var sexp_list * var typeDefRight sexp_list) Base.Hashtbl.t)  =
    let result = Hashtbl.create strIntKeyImp
    in let () = Hashtbl.iteri ~f:(fun ~key:k ~data:(vars,cases) ->
        List.iter
          ~f:(fun (TypeDefRight(name,args)) -> Hashtbl.add_exn result ~key:(name,List.length args) ~data:(k,vars,args)) cases)
        typeDefs
    in let () = logDebug (fun m -> m "constructor to type is")
    in let () = Hashtbl.iteri
           ~f:(fun ~key:(v,i) ~data:_ -> logDebug (fun m -> m "%a %a body?" pp_var v pp_intnum i))
           result
    in let () = logDebug (fun m -> m "")
    in result
