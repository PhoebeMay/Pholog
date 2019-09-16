open Dt
open Core
open Logging
open FlattenInstrForPrint
open Unification
open RuntimeDataStructures
open StackImplementation
open EnvCpStackOps
open InstructionImplementationsFunctor

module type DriverAndFunctionsTyp =
sig

  module DriverImpl : sig
    val execute : compState -> environment option
  end

end

module type StateTyp =
sig
  val instructions : instruction sexp_array
  val resCount : int ref
  val structMap : structureLookup
end


module DriverAndFunctions (State : StateTyp)  : DriverAndFunctionsTyp = struct

  module Driver  (I : InstructionFunctionsTyp)  : DriverTyp = struct

    let rec execute compState : 'a option =
      logDebug (fun m -> m "%a . %a" pp_intnum (compState.cp)
                   pp_instruction (Array.get State.instructions compState.cp));
      logDebug (fun m -> m "CP stack length: %a" pp_intnum
                   (getLength compState.choicePoints));
      (* Old code for collecting run-time statistics *)
      (* let ss = (Gc.stat() ).stack_size
         in let () = logError (fun m -> m "%a" pp_intnum ss) *)
      (* in *)
      (*   itercount := !itercount+1;
           if ((!itercount mod 1000000) = 0)
           then (
           let now = Int63.to_string (Time_ns.to_int63_ns_since_epoch (Time_ns.now()))
           in let heap = (Gc.stat()).live_words
           in logError (fun m -> m "(%s,%a),"now pp_intnum heap))
           else (); *)
      (* in  *)
      let cp = compState.cp
      in let choicePoints = compState.choicePoints
      in let envStack = compState.envStack
      in
      match ((Array.get State.instructions cp), choicePoints, envStack) with

      | TryMeElse(PositionC(n)),_, _ ->
        (*Set up a choice point*)
        let newChoicePoint = {
          nextOptionPointer=n;
          stack = envStack;
          arguments = (Array.copy compState.arguments);
          returnAddr = compState.returnAddress;
          returnCps = compState.returnCps;
          returnTrailPoint = compState.returnTrailPoint;

        }
        in compState.trail <- createNew Label ~prev:compState.trail;
        compState.choicePoints <-
          Some({prev = choicePoints; value = newChoicePoint});
        compState.cp <- compState.cp + 1 ;
        execute compState

      | RetryMeElse(PositionC(n)), Some(c), _ ->
        (*Update a choice point*)
        updateChoicePoint n c.value;
        compState.trail <- createNew Label ~prev:compState.trail;
        compState.cp <- compState.cp + 1 ;
        execute compState

      | TrustMe, Some(c), _ ->
        (*Discard a choice point*)
        compState.cp <- compState.cp + 1 ;
        compState.choicePoints <- c.prev;
        execute compState

      | GetVariable(position,Arg(a)), _, Some(env) ->
        let loadedVar = Array.get compState.arguments a
        in let () = logDebug (fun m -> m "getvariable %a" pp_variable loadedVar)
        in (match position with
            | T(Temp(e)) -> Array.set compState.temps e loadedVar
            | E(Env(e)) -> Array.set env.value.vars e loadedVar
          );
        compState.cp <- compState.cp + 1 ;
        execute compState

      | ClearTemp(Temp(t)),_,_ ->
        let newHeapVar = HeapPointer(B( (ref UnboundVar)))
        in
        Array.set (compState.temps) t newHeapVar;
        compState.cp <- compState.cp + 1;
        execute compState

      | GetInt(Arg(a),n1),_,_ ->
        let loadedArg = Array.get compState.arguments a
        in let () = logDebug (fun m -> m
                                 "GetInt loaded var %a" pp_variable loadedArg)
        in I.getinta loadedArg n1 compState


      | GetValue(position,Arg(a)), _, Some(env) ->
        let argVar = Array.get compState.arguments a
        in let envVar = match position with
            | E(Env(e)) -> Array.get env.value.vars e
            | T(Temp(t)) -> Array.get compState.temps t
        in let oldCp = compState.cp
        in let newState = unifyS envVar argVar compState
        in
        if newState.cp = oldCp
        then (compState.cp <- compState.cp + 1 ;
              execute newState)
        else execute newState

      | GetStructureA((name,num),Arg(a)), _, _ ->
        I.getStructure name num compState.arguments a compState

      | GetStructureT((name,num),Temp(t)), _, _ ->
        I.getStructure name num compState.temps t compState

      | Allocate(n), _, (stack : stackVal) ->
        (*Allocate a new environment*)
        compState.cp <- compState.cp + 1;
        let newEnv =
          match stack with
          | Some(e) -> (createEnvironment n compState.returnAddress
                          (Some(e)) compState.returnCps
                          compState.returnTrailPoint)
          | None -> (createEnvironment n compState.returnAddress
                       None compState.returnCps compState.returnTrailPoint)
        in
        compState.envStack <- newEnv;
        execute compState

      | DeallocateBeforeLastCall, _, Some(stack) ->
        (*Discard an environemnt*)
        let newStackVal = match stack.prev with
          | Some(prev) -> Some(prev)
          | None -> raise Oops
        in
        compState.returnAddress <-  (stack.value.returnAddress) ;
        compState.envStack <- newStackVal;
        compState.cp <-  compState.cp + 1;
        execute compState

      | Deallocate, _, Some(stack) ->
        (*Discard an environemnt*)
        let newStackVal = match stack.prev with
          | Some(prev) -> Some(prev)
          | None -> raise Oops
        in

        logDebug(fun m -> m "cp is %a" pp_intnum compState.cp);
        logDebug(fun m -> m "cp set to %a" pp_intnum (stack.value.returnAddress));
        compState.cp <- (stack.value.returnAddress) ;
        compState.envStack <- newStackVal;
        execute compState

      | PutVariable(location,Arg(a)), _, Some(env) ->
        (*Put a new unbound variable on the heap and copy it into Vn and Ai*)
        let newHeapVar = B( (ref UnboundVar))
        in
        let () = (match location with
            | T(Temp(t)) -> Array.set compState.temps t (HeapPointer(newHeapVar))
            | E(Env(e)) -> Array.set env.value.vars e (HeapPointer(newHeapVar))
          )
        in
        Array.set compState.arguments a (HeapPointer(newHeapVar));
        compState.cp <- compState.cp + 1;
        execute compState

      | PutValue(location,Arg(s)),_, Some(env) ->
        let toPut = (
          match location with
          | E(Env(e)) -> Array.get env.value.vars e
          | T(Temp(t)) -> Array.get compState.temps t
        )
        in Array.set compState.arguments s toPut;
        compState.cp <- compState.cp + 1;
        execute compState

      | PutStructureA((name,num),Arg(a)), _, _ ->
        I.putStructure name num compState.arguments a compState

      | PutStructureT((name,num),Temp(t)), _, _ ->
        I.putStructure name num compState.temps t compState

      | SetInt(num),_,_ ->
        let (argArray,pos) = compState.currentStr
        in let newHeapVar = (Int num)
        in let () = Array.set argArray pos newHeapVar;
             compState.currentStr <- (argArray, pos + 1);
             logDebug (fun m -> m "    Setint gives new argarray %a" pp_heap argArray)
        in
        compState.cp <- compState.cp + 1;
        execute compState

      | SetVariable(E(Env(e))), _, Some(env) ->
        (*Put a new ref cell on the end of the current arguments being built*)
        (*This is used to build arguments to a function*)
        (*Copy it into the stack variable e*)
        let (argArray,pos) = compState.currentStr
        in let newHeapVar = B( (ref UnboundVar))
        in let () = Array.set argArray pos newHeapVar
        in let () = Array.set env.value.vars e (HeapPointer(newHeapVar))
        in
        compState.cp <- compState.cp + 1;
        execute compState


      | SetValue(E(Env(e))), _, Some(env) ->
        I.setValue env.value.vars e compState

      | SetValue(T(Temp(e))), _,_->
        I.setValue compState.temps e compState

      | StructGetInt(n1), _, _ ->
        (* This is calling unify, can i refactor somehow *)
        let (heapValueArray,pos) = compState.currentStr
        in let heapTop = Array.get heapValueArray pos
        in let oldCp = compState.cp
        in let newState = unifyH heapTop (Int(n1)) compState
        in
        if newState.cp = oldCp
        then (
          newState.currentStr <- (heapValueArray, pos+1);
          newState.cp <- compState.cp + 1;
          execute newState
        )
        else execute newState

      | StructGetValue(position),_,Some(env) ->
        (let (heapValueArray,pos) = compState.currentStr
         in let heapTop = Array.get heapValueArray pos
         in let envVar = match position with
             | E(Env(e)) -> Array.get env.value.vars e
             | T(Temp(t)) -> Array.get compState.temps t

         in let () = logDebug (fun m -> m " StructGetValue %a <- %a "
                                  pp_variable envVar pp_heapValue heapTop )

         in let oldCp = compState.cp
         in let newState = unifyS envVar (HeapPointer(heapTop)) compState
         in
         let () = logDebug (fun m -> m "%a %a"
                               pp_intnum oldCp pp_intnum newState.cp)
         in
         if newState.cp = oldCp
         then (
           newState.currentStr <- (heapValueArray, pos+1);
           newState.cp <- compState.cp + 1;
           execute newState
         )
         else execute newState
        )

      | StructGetVariable(E(Env(e))),_, Some(env) ->
        I.unifyInFromHeap (env.value.vars) e compState

      | StructGetVariable(T(Temp(e))),_, Some(env) ->
        I.unifyInFromHeap compState.temps e compState

      | PutIntT(Temp(t), n),_,_ ->
        let () = Array.set compState.temps t (TempInt(n));
          compState.cp <- compState.cp + 1
        in execute compState

      | AddI(Temp(t),n),_,_ ->
        let oldVal = Array.get compState.temps t
        in (match oldVal with
            | TempInt(x) ->
              let () = Array.set compState.temps t (TempInt(n + x));
                compState.cp <- compState.cp + 1
              in execute compState
            | _ -> raise (Err "Expected accumilator to be an int")
          )

      | SubI(Temp(t),n),_,_ ->
        let oldVal = Array.get compState.temps t
        in (match oldVal with
            | TempInt(x) ->
              let () =
                Array.set compState.temps t (TempInt(x - n));
                compState.cp <- compState.cp + 1
              in execute compState
            | _ -> raise (Err "Expected accumilator to be an int")
          )

      | Add(Temp(accu),position),_,Some(env) ->
        I.performArith compState position accu env (fun x y -> x + y)

      | Sub(Temp(accu),position),_,Some(env) ->
        I.performArith compState position accu env (fun x y -> x - y)

      | PutIntA (Arg(a), n),_,_ ->
        let newHVal = HeapPointer (Int n)
        in let () = Array.set compState.arguments a newHVal
        in compState.cp <- compState.cp + 1;
        execute compState

      | InitAcc(Temp(t),position),_,Some(env) ->
        let envV = match position with
          | E(Env(e)) -> Array.get env.value.vars e
          | T(Temp(t)) -> Array.get compState.temps t
        in  (
          match envV with
          | HeapPointer(Int(n)) -> (
              Array.set compState.temps t (TempInt(n));
              compState.cp <- compState.cp + 1;
              execute compState
            )
          |  HeapPointer(B(hp)) -> (
              match !(hp) with
              | Int(n) -> Array.set compState.temps t (TempInt(n));
                compState.cp <- compState.cp + 1;
                execute compState
              | other -> raise (Err("Init acc " ^ show_heapValue other))
            )
          | x -> raise (
              Err("Try to initialise accumilator to " ^(show_variable x))
            )
        )

      | Is(position,Temp(t)),_,Some(env) ->
        (match position with
         | E(Env(e)) -> let envVar = Array.get env.value.vars e;
           in  I.evalIs envVar t env.value.vars e compState
         | T(Temp(tmp)) -> let var = Array.get compState.temps tmp
           in I.evalIs var t compState.temps tmp compState);

      | Call(PositionF(addr)), _, _ ->
        let () =
          compState.returnAddress <- compState.cp + 1;
          compState.cp <- addr;
          compState.returnCps <- choicePoints;
          compState.returnTrailPoint <- compState.trail
        in let _ =
             if List.exists (getPreds (compState.returnCps))
                 ~f:(fun x -> Some(x) = compState.choicePoints)
             then raise Oops
             else ()
        in execute compState

      | CallAfterDealloc(PositionF(addr)),_,_ ->
        compState.cp <- addr;
        compState.returnCps <- choicePoints;
        compState.returnTrailPoint <- compState.trail;
        execute compState

      | RmCps, _,Some(e) ->
        compState.cp <- compState.cp + 1;
        compState.choicePoints <- e.value.callerCps;
        compState.trail <- e.value.callerTrailpoint;
        let resetHead = e.value.callerCps
        in execute compState

      | Backtrack,_,_ ->
        execute (backtrack compState)

      | Finish, _, Some(e) ->
        (match !State.resCount with
         | 1 -> Some(e.value)
         | n -> State.resCount := n-1;
           let thisans = e.value.vars
           in let flat =  List.map
                  ~f:(flattenSv State.structMap) ( Array.to_list thisans)
           in logError (fun m -> m "%a \n" pp_res flat);
           execute ( backtrack compState)
        )
      | other,_,_ ->
        raise (InstrNotMatch(show_instruction other))

  end
  module rec DriverImpl : DriverTyp
    = Driver(InstructionImplementationsImpl)
  and InstructionImplementationsImpl : InstructionFunctionsTyp
    = InstructionImplementations (DriverImpl)
end



let executeInstructions instructions initialState structMap =
  let module State : StateTyp = struct
    let instructions = instructions
    let resCount = ref 1
    let structMap = structMap
  end
  in let runModule = (module DriverAndFunctions (State) : DriverAndFunctionsTyp)
  in let module DriverAndFunctionsM  = (val runModule : DriverAndFunctionsTyp)
  in DriverAndFunctionsM.DriverImpl.execute initialState
