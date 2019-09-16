open Dt
open Core
open Logging
open Unification
open RuntimeDataStructures
open StackImplementation

module InstructionImplementations (D : DriverTyp) : InstructionFunctionsTyp =
struct
  let performArith compState position accu env arithfun : 'a option =
    let oldVal = compState.temps.(accu) in
    let envVal =
      match position with
      | E (Env e) -> env.value.vars.(e)
      | T (Temp t) -> compState.temps.(t)
    in
    match (oldVal, envVal) with
    | TempInt x, HeapPointer (B hp) -> (
        match !hp with
        | Int y ->
            compState.temps.(accu) <- TempInt (arithfun x y);
            compState.cp <- compState.cp + 1;
            D.execute compState
        | _ -> raise (Err "Add non-int var") )
    | TempInt x, HeapPointer (Int y) ->
        compState.temps.(accu) <- TempInt (arithfun x y);
        compState.cp <- compState.cp + 1;
        D.execute compState
    | _ -> raise (Err "opsoo")

  let rec evalIs envVar t place numset compState =
    match envVar with
    | HeapPointer x -> evalIsHeap x t compState
    | InitV -> (
        let temp = compState.temps.(t) in
        match temp with
        | TempInt n ->
            let newEnv = HeapPointer (Int n) in
            let () = place.(numset) <- newEnv in
            compState.cp <- compState.cp + 1;
            D.execute compState
        | y -> raise (Err (show_variable y)) )
    | other -> raise (Err (show_variable other))

  and evalIsHeap x t compState =
    (* logDebug (fun m -> m "eval is heap, x is %a " pp_heapValue x); *)
    match x with
    | B hp -> (
        match !hp with
        | UnboundVar -> (
            let temp = compState.temps.(t) in
            match temp with
            | TempInt n ->
                let newTrail = createNew (Var hp) ~prev:compState.trail in
                hp := Int n;
                compState.cp <- compState.cp + 1;
                compState.trail <- newTrail;
                D.execute compState
            | y -> raise (Err (show_variable y)) )
        | other -> evalIsHeap other t compState )
    | Int n1 -> (
        let temp = compState.temps.(t) in
        match temp with
        | TempInt n2 ->
            (* logDebug (fun m -> m "eval is heap int expected is %a"pp_intnum n2); *)
            if n1 = n2 then (
              compState.cp <- compState.cp + 1;
              D.execute compState )
            else D.execute (backtrack compState)
        | y -> raise (Err (show_variable y)) )
    | y -> raise (Err (show_heapValue y))

  let rec getinta loadedArg n1 compState : environment option =
    match loadedArg with
    | HeapPointer hp -> getintah hp compState n1
    | other -> raise (Err (show_variable other))

  and getintah hval compState n1 : environment option =
    match hval with
    | Int n2 ->
        if n1 = n2 then (
          compState.cp <- compState.cp + 1;
          D.execute compState )
        else D.execute (backtrack compState)
    | B hv -> (
        match !hv with
        | UnboundVar ->
            let newTrail = createNew (Var hv) ~prev:compState.trail in
            hv := Int n1;

            (* logDebug (fun m ->  m "Bind makes %a " pp_variable loadedArg); *)
            (* logDebug (fun m -> m "getint binding unboundvar to %a" pp_intnum n1 ); *)
            compState.cp <- compState.cp + 1;
            compState.trail <- newTrail;
            D.execute compState
        | other -> getintah other compState n1 )
    | _ -> raise Oops

  let rec getStructure name num location position compState =
    let vari =
      location.(position)
      (* in let () =    logDebug (fun m -> m "    getStructure gets %a " pp_variable vari ) *)
    in
    match vari with
    | HeapPointer h -> getStructureH h name num compState
    | _ -> raise Oops

  and getStructureH hval name num compState =
    logDebug (fun m -> m "get structureH %a " pp_heapValue hval);
    match hval with
    | StrPointer (name2, args) ->
        if name = name2 && num = Array.length args then (
          compState.currentStr <- (args, 0);
          compState.cp <- compState.cp + 1;
          D.execute compState )
        else (
          logDebug (fun m -> m "Unify in get structure failed 1");
          D.execute (backtrack compState) )
    | B r -> (
        match !r with
        | UnboundVar ->
            (*IS THIS THE RIGHT INITIAL STATEE *)
            let newArray = Array.init num ~f:(fun _ -> B (ref UnboundVar)) in
            let newTrail = createNew ~prev:compState.trail (Var r) in
            r := StrPointer (name, newArray);

            (* TODO does this change? *)
            compState.currentStr <- (newArray, 0);

            (* Do I need to add to the trail stack?? *)
            compState.cp <- compState.cp + 1;
            compState.trail <- newTrail;
            logDebug (fun m -> m "Trail bind ");
            D.execute compState
            (* raise Oops *)
        | other ->
            logDebug (fun m -> m "recursive call getstructh");
            getStructureH other name num compState )
    | _ ->
        logDebug (fun m -> m "Unify in get structure failed");
        D.execute (backtrack compState)

  let putStructure name num location position compState =
    let strArgs = Array.create ~len:num InitH in
    let () = compState.currentStr <- (strArgs, 0) in
    let newHVal = StrPointer (name, strArgs) in
    let () =
      location.(position) <- HeapPointer newHVal
      (* logDebug (fun m -> m "    put structure %a " pp_varr location) *)
    in
    compState.cp <- compState.cp + 1;
    D.execute compState

  let unifyInFromHeap arr arrpos compState =
    let heapValueArray, pos = compState.currentStr in
    let heapTop = heapValueArray.(pos) in
    arr.(arrpos) <- HeapPointer heapTop;
    compState.currentStr <- (heapValueArray, pos + 1);
    compState.cp <- compState.cp + 1;
    D.execute compState

  let setValue location position compState =
    (*Push the value of environment variable e onto the heap*)
    let envV = location.(position) in
    match envV with
    | InitV -> raise Oops
    | TempInt _ -> raise (Err "environment variable shouldn't be a tempint ")
    | HeapPointer x ->
        let argArray, pos = compState.currentStr in
        let () =
          argArray.(pos) <- x;
          compState.currentStr <- (argArray, pos + 1)
        in
        compState.cp <- compState.cp + 1;
        D.execute compState
end
