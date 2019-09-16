open Core
open ParseTree
open Dt
open Logging
open GetDeclarations
open InferVariableTypes
open CheckClause
open CheckTypeEquivalence

(* print current bindings  *)
let logBindings binding =
  logDebug (fun m -> m "bindings are");
  Hashtbl.iteri
    ~f:(fun ~key:k ~data:v ->
        (logDebug (fun m -> m "(%a,%a)\n" pp_typei k pp_infv v)))
    binding

type argtypelist = (var * intnum) typeins list [@@deriving show]

let typeCheckClause clause predTypes typeDefs constructorToType =
  let (Clause(Atom(name,args),body)) = clause
  in let () = logDebug
         (fun m -> m "TYPE CHECK CLAUSE: typeCheckClause %a \n" pp_var name)
  in let () = Hashtbl.iteri
         ~f:(fun ~key:k ~data:_ -> (logDebug (fun m -> m "%a" pp_var k)))
         predTypes

  in let predType = Hashtbl.find_exn predTypes name
  in let variableTypes =
       getTypesOfVariables clause predTypes typeDefs constructorToType
  in let argTypeList =
       getPredicateArgumentTypes args variableTypes constructorToType

  in let () = logDebug
         (fun m -> m "Arg type  list is %a" pp_argtypelist argTypeList)
  in let () = checkTypesAreEquivalent argTypeList predType
  in let () = logDebug(fun m -> m "Arg types are equivalent ")

  in let binding =  (Hashtbl.create strIntKeyImp)
  in let () = List.iter
         ~f:(typeCheckCBV predTypes variableTypes constructorToType binding)
         body
  in let () = logBindings binding
  in ()

let typeCheckQuery query predTypes  (constructorToType)=
  let () = logDebug (fun m -> m "type check query\n\n\n\n")
  in let () = Hashtbl.iteri
         ~f:(fun ~key:k ~data:_ -> (logDebug (fun m -> m "%a" pp_var k)))
         predTypes
  in let variableTypes =
       getTypesOfVariablesQuery query predTypes constructorToType
  in let binding =  (Hashtbl.create strIntKeyImp)
  in let () =
       List.iter
         ~f:(typeCheckCBV predTypes variableTypes constructorToType binding)
         query
  in ()


let checkPredTypesValid predTypes typeDefs =

  let checkTypeInsAppears x =
    match x with
    | TypeCons(name,args) ->
      (match Hashtbl.find typeDefs name
       with Some(_) -> ()
          | None ->
            raise (Err ("Could not find definition for "^name
                        ^(show_intnum (List.length args)))))
    | TypeVar(_) -> ()
    | IntTyp -> ()
  in
  Hashtbl.iteri
    ~f:(fun ~key:_ ~data:d -> List.iter ~f:checkTypeInsAppears d) predTypes


let typeCheck tc (Program(Sentence(body),Resolvant(goals))) =
  if not tc
  then logInfo (fun m -> m "Type checking disabled\n")
  else
    (  logInfo (fun m -> m "Type checker about to run\n");

       let predTypes = getPredTypes body
       in let typeDefs = getTypeDefs body
       in let (constructorToType ) = reverse typeDefs
       in checkPredTypesValid predTypes typeDefs;
       List.iter
         ~f:(fun x ->
             match x with
               C(x) -> typeCheckClause x
                         predTypes
                         typeDefs
                         constructorToType
             | _ -> ())
         body;
       typeCheckQuery goals predTypes constructorToType;
       logInfo (fun m -> m "Type checked sucessfully\n")
    )
