open Core
open Dt
open Logging
open RuntimeDataStructures

let getInitialState arrLens =
  let currentStr = ((Array.create ~len:0 InitH),0)
  in let cp = 0
  in let returnAddress = (-1 )
  in let returnCps = None
  in let returnTrailPoint = None

  in let arguments = (Array.create ~len:(arrLens.maxArg) InitV)
  in let temps = Array.create ~len:(arrLens.maxTemp) InitV
  in let () = logDebug (fun m -> m "temps length is %a" pp_intnum arrLens.maxTemp)

  in let initialState = {
      currentStr = currentStr;
      cp = cp;
      returnAddress = returnAddress;
      returnCps = returnCps;
      returnTrailPoint = returnTrailPoint;
      arguments = arguments;
      temps = temps;
      choicePoints = None;
      envStack = None;
      trail = (StackImplementation.createNew Label ~prev:None);
      infinal = true;
    }
in initialState
