open Core
open Dt

let rec getInstructionNumClauseCodes clauseCodes =
  match clauseCodes with
  | [] -> 0
  | ((_pos,clauseInstructions)::cs) -> List.length clauseInstructions + getInstructionNumClauseCodes cs

let rec getInstructionNumPredCodes predCodes =
  match predCodes with
  | [] -> 0
  | ((_id : functionId),xs)::ys -> getInstructionNumClauseCodes xs + getInstructionNumPredCodes ys


type clauseId = functionId * int [@@deriving show, sexp]


(* hash table implementation for clause id *)
let clauseIdImp =
  (module struct
    type t = clauseId
    let compare = Pervasives.compare
    let sexp_of_t = sexp_of_clauseId
    let hash (x,pos) = match x with
      | PositionF(n) -> n
      | AbstractF(name,arity) ->
        let hash = Hash.create ~seed:(String.hash name) ()
        in let hash2 = Hash.fold_int hash arity
        in let hash3 = Hash.fold_int hash2 pos
        in Hash.get_hash_value hash3
  end : Base__.Hash_set_intf.Key with type t = clauseId)



let putIntoArray instructions positionT=
  let size = getInstructionNumPredCodes instructions
  in let codeArray = Array.create ~len:size Empty

  in let rec putInstructionsInArray pos instructions =
       match instructions with
       | (i::is) ->
         Array.set codeArray pos i;
         putInstructionsInArray (pos+1) is
       | [] -> ()

  in let rec putClauseCodesInArray (clauseCodes ) id (currentPosition : int) =
       match clauseCodes with
       | [] -> currentPosition
       | ((pos,clauseInstructions)::cs) ->
         Hashtbl.add_exn positionT ~key:(id,pos) ~data:currentPosition;
         putInstructionsInArray currentPosition clauseInstructions;
         putClauseCodesInArray cs id (currentPosition + List.length clauseInstructions)

  in let rec putPredCodesInArray (predCodes) (currentPosition : int) =
       match predCodes with
       | [] -> ()
       | ((id : functionId),xs)::ys ->
         let pos2 = putClauseCodesInArray xs id currentPosition
         in putPredCodesInArray ys pos2

  in putPredCodesInArray instructions 0; codeArray


let renameInstruction i positionT=
  match i with
  | Call(AbstractF(fn)) ->
    let pos = Hashtbl.find positionT (AbstractF(fn),0)
    in (match pos with
    | Some(p) -> Call(PositionF(p))
    | None -> raise (
        Err ("Attempt to call predicate that doesn't exist " ^
             (show_functionId (AbstractF(fn))) ) )
      )
  | CallAfterDealloc(AbstractF(fn)) ->
    let pos = Hashtbl.find positionT (AbstractF(fn),0)
    in (match pos with
        | Some(p) -> CallAfterDealloc(PositionF(p))
        | None -> raise (
            Err ("Attempt to call predicate that doesn't exist " ^
                 (show_functionId (AbstractF(fn))) ) )
      )
  |  TryMeElse(AbstractC(AbstractF(fn),num)) ->
    let pos = Hashtbl.find_exn positionT (AbstractF(fn),num)
    in TryMeElse(PositionC(pos))
  | RetryMeElse(AbstractC(AbstractF(fn),num)) ->
    let pos = Hashtbl.find_exn positionT (AbstractF(fn),num)
    in RetryMeElse(PositionC(pos))
  | a -> a

let renameArray positionT instructionArray =
  Array.map_inplace
    ~f:(fun i -> renameInstruction i positionT) instructionArray;
  instructionArray

let lineariseCode is =
  let positionT = Hashtbl.create clauseIdImp
  in
  (renameArray positionT) (putIntoArray is positionT)
