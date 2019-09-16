open Core
open ParseTree
open Dt
open Pervasives
open CodeStructureInArg
open CodeLoadStructure
open GenFunctionTable
open Utils
open Linearise

type functTval = (location ParseTree.clause * intnum) list [@@deriving sexp]
type query = location ParseTree.clauseBodyVal list * intnum [@@deriving sexp]
type code = (functionId * (int * instruction list) list) list
            * Dt.arrLens
            * Dt.structureLookup [@@deriving sexp]
type functTlist = (functionId * functTval) list [@@deriving sexp ]


(* Load an argument *)
let codeArgTerm (arg : location term) argnum seenSoFar nextFreeTemp
    structMapGen =
  match arg with
  | TInt(n) -> [GetInt(Arg(argnum),n)]
  | TVar(var) -> (
      if (seen var seenSoFar)
      then [GetValue(var, Arg(argnum))]
      else [GetVariable(var,Arg(argnum))]
    )
  | TFun(funct, ts) ->
    GetStructureA(
      (getStructMapInt structMapGen funct, List.length ts),
      Arg(argnum)
    )
    :: (codeArgStructs ts nextFreeTemp seenSoFar structMapGen)

(* Load all arguments *)
let rec codeArgs ars counter seenSoFar nextFreeTemp structMapGen =
  match ars with
  | [] -> []
  | ((a:location term)::args) ->
    let code1 = codeArgTerm a counter seenSoFar nextFreeTemp structMapGen
    in let code2 =  codeArgs args (counter + 1) seenSoFar nextFreeTemp
           structMapGen
    in code1 @ code2


let loadArg argnum seenSoFar nextFreeTemp structMapGen (term : location term) =
  match term with
  | TInt(n) -> [PutIntA(Arg(argnum), n)]

  | TVar(var) ->
    if (seen var seenSoFar)
    then [PutValue(var, Arg(argnum))]
    else [PutVariable(var,Arg(argnum))]

  | TFun(funct, ts) ->
    let (build,load) = loadAllFunBody ts seenSoFar nextFreeTemp structMapGen
    in
    build
    @ [PutStructureA(
        (getStructMapInt structMapGen funct, List.length ts),
        Arg(argnum))
      ]
    @ load

let loadArgs (terms : location term list) (seenSoFar : location Hash_set.t)
    nextFreeTemp structMapGen =
  let argnum = ref 0
  in
  let ans  = List.concat
      (List.map
         ~f:(fun t -> argnum := !argnum + 1;
              loadArg (!argnum - 1) seenSoFar nextFreeTemp structMapGen t)
         terms);
  in ans

let rec codeArith seenSoFar codeArithT (item : location mathExpr) =
  match item with
  | Plus(exp1, Base(Int(n))) ->
    (codeArith seenSoFar codeArithT exp1)
    @ [AddI(codeArithT,n)]

  | Plus(exp1, Base(Var(v))) ->
    (codeArith seenSoFar codeArithT exp1)
    @ [Add(codeArithT,v)]

  | Subtract(exp1, Base(Int(n))) ->
    (codeArith seenSoFar codeArithT exp1)
    @ [SubI(codeArithT,n)]

  | Subtract(exp1, Base(Var(v))) ->
    (codeArith seenSoFar codeArithT exp1)
    @ [Sub(codeArithT,v)]

  | Base(Int(n)) -> [PutIntT(codeArithT, n)]
  | Base(Var(v)) -> [InitAcc(codeArithT, v) ]
  | _ -> raise (Err "Parse tree malformed, addition left assoc")


let codeBodyVal (seenSoFar : location Hash_set.t) nextFreeTemp structMapGen clauseBodyItem=
  match clauseBodyItem with
  | CAT(Atom(pred, terms)) ->
    (loadArgs terms seenSoFar nextFreeTemp structMapGen)
    @ [Call(AbstractF(pred, List.length terms))]
  | Cut -> [RmCps]
  | Fail -> [Backtrack]
  | CAR(IsExpr(var,body)) ->
    let codeArithT = Temp(!nextFreeTemp)
    in let () =  nextFreeTemp := !nextFreeTemp + 1
    in let bodyCode = codeArith seenSoFar codeArithT body
    in let () = Hash_set.add seenSoFar var
    in
    bodyCode @ [Is(var,codeArithT)]


let codeBody (bodyVals : location clauseBodyVal list)
    (seenSoFar : location Hash_set.t) nextFreeTemp structMapGen =
  match bodyVals with
  | [] -> []
  | _ -> List.concat (List.map ~f:(codeBodyVal seenSoFar nextFreeTemp structMapGen) bodyVals)


(* Resturn (last element, original list) *)
let rec firstLast xs res =
  match xs with
  | [] -> raise Oops
  | x::[] -> x,(List.rev res)
  | y::ys -> firstLast ys (y::res)

(* Get the highest tempoary from a term *)
let rec getMaxTempTerm result term =
  match term with
  | TVar(T(Temp(pos))) ->
    if pos > !result
    then result := pos
    else ()
  | TFun(_, args) ->
    List.iter
      ~f:(getMaxTempTerm result)
      args
  | _ -> ()

let getMaxTempAtom (Atom(_,args)) result =
  List.iter ~f:(getMaxTempTerm result) args

let rec getMaxTempMathExpr result mathExpr =
  match mathExpr with
  | Plus(x,y) ->
    getMaxTempMathExpr result x;
    getMaxTempMathExpr result y
  | Subtract(x,y) ->
    getMaxTempMathExpr result x;
    getMaxTempMathExpr result y
  | Base(Int(_)) -> ()
  | Base(Var(T(Temp(pos)))) ->
    if pos > !result
    then result := pos
    else ()
  |Base(Var(E(_))) -> ()

let getMaxTempCBV result cbv =
  match cbv with
  | Cut -> ()
  | Fail -> ()
  | CAT(a) -> getMaxTempAtom a result
  | CAR(IsExpr(vari,mathexpr)) ->
    getMaxTempMathExpr result mathexpr;
    match vari with
    | T(Temp(pos)) -> if pos > !result then result := pos else ()
    | _ -> ()

let rec getMaxTempClause (Clause(head,tail)) =
  let result = ref 0
  in
  getMaxTempAtom head result;
  List.iter ~f:(getMaxTempCBV result) tail;
  !result



and addTempClears defined instructions =
  match instructions with
  |  [] -> ([]:instruction list)
  | i::is -> (
      match i with
      | GetVariable(T(t),_) ->
        Hash_set.add defined (T(t));
        i::(addTempClears defined is)

      | StructGetVariable(T(t)) ->
        if (not (
            Hash_set.find defined ~f:(fun x -> x = T(t)) = None
          ))
        then (
          i::(addTempClears defined is))
        else (Hash_set.add defined (T(t));
              i::(addTempClears defined is)
             )

      | InitAcc(_, T(t)) ->
        if (not
              (Hash_set.find defined ~f:(fun x -> x = T(t)) = None)
           )
        then i::(addTempClears defined is)
        else
          (Hash_set.add defined (T(t));
           (ClearTemp t)::i::(addTempClears defined is)
          )

      | Is(T(t),_) ->
        if (not (Hash_set.find defined ~f:(fun x -> x = T(t)) = None))
        then i::(addTempClears defined is)
        else (Hash_set.add defined (T(t));
              (ClearTemp t)::i::(addTempClears defined is)
             )
      | _ -> i::(addTempClears defined is)
    )

let codeClause ((clause,n) : location clause * int) arrLens structMapGen =
  let seen = Base.Hash_set.create locationImp
  in let maxTemp = getMaxTempClause clause
  in let nextFreeTemp = ref (maxTemp + 1)

  in let codeInsideClause (args : location term list) tail =
       let load =  (codeArgs args 0 seen nextFreeTemp structMapGen)
       in let call = (codeBody tail seen nextFreeTemp structMapGen)
       in let argNum = List.length args
       in let tempNum = !nextFreeTemp
       in let () = arrLens.maxArg <- max argNum arrLens.maxArg
       in let () = arrLens.maxTemp <- max tempNum arrLens.maxTemp
       in
       load
       @ call

  in let result = match (clause,n) with
        (Clause(Atom(_name, args), tail),n) ->
        let bodycode = (codeInsideClause args tail )
        in if List.length bodycode > 0
        then (
          let (t,hs) = firstLast bodycode []
          in match t with
          | Call(x) ->
            Allocate n :: hs
            @  [DeallocateBeforeLastCall]
            @ [CallAfterDealloc(x)]
          | _ ->
            Allocate n :: hs
            @ [t]
            @ [Deallocate])
        else Allocate n :: [Deallocate]

  in
  addTempClears (Hash_set.create locationImp ())
    result

let rec codeNotFirstClauses id clauses counter arrLens structMapGen=
  match clauses with
  | [] -> raise Oops
  | [c] -> let r = [(counter,TrustMe :: codeClause c arrLens structMapGen )]
    in r
  | c::cs ->
    let head = (RetryMeElse(AbstractC(id,counter+1))
                :: codeClause c arrLens structMapGen)
    in (counter,head)
       ::codeNotFirstClauses id cs (counter + 1) arrLens structMapGen

let codeFirstClause id (clauses) arrLens structMapGen=
  match clauses with
  | [] -> raise Oops
  | [c] -> [(0, codeClause c arrLens structMapGen)]
  | c::cs ->
    let head = (TryMeElse(AbstractC(id,1))
                :: codeClause c arrLens structMapGen)
    in (0,head)
       :: (codeNotFirstClauses id cs 1 arrLens structMapGen)

let codePred id
    (functionTable : (functionId, (location clause * int) sexp_list) Base.Hashtbl.t)
    arrLens structMapGen =

  let clauses = List.rev (Hashtbl.find_exn functionTable id)
  in (id, codeFirstClause id clauses arrLens structMapGen)

let codeAllClauses (functionTable : (functionId, (location clause * int) sexp_list) Base.Hashtbl.t)
    arrLens structMapGen =
  let fids = Hashtbl.keys functionTable
  in List.map ~f:(fun id -> codePred id functionTable arrLens structMapGen) fids

let codeQuery query arrLens structMapGen=
  let seen = Base.Hash_set.create locationImp
  in let nextFreeTemp = ref 0
  in match query with (body,n)->
    let res = [
      (AbstractF("query",0),
       [ (
         0,
         Allocate n :: codeBody body seen nextFreeTemp structMapGen
         @ [Finish]
       )])
    ]
    in arrLens.maxTemp <- max arrLens.maxTemp !nextFreeTemp;
    res

let genCode parseTree =
  let functionTable,query = genFunctionTable parseTree

  in let arrLens = {maxArg = 0; maxTemp = 0}
  in let structMapGen = {mapping = Hashtbl.create strKeyImp; nextFree = 0}
  in
  let querycode = (codeQuery query arrLens structMapGen)
  in let bodycode = (codeAllClauses functionTable arrLens structMapGen)
  in let code = querycode @ bodycode
  in let lookupmap =  reverseStructmap structMapGen
  in let flatcode = lineariseCode code
  in let instr = {code=flatcode; nums=arrLens; structMap = lookupmap}
  in instr
