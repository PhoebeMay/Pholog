open Core
open Dt
open Execute
open Initialise
open RuntimeDataStructures

let runProgram ({nums=arrLens; code=instructions; structMap = structMap}
                : writtenInstr)
  : 'a option=
  try(

    let is = getInitialState arrLens
    in let result = executeInstructions instructions is structMap
    in match result with
    |None -> None
    | Some(e) -> Some(e.vars)
  )
  with Fail -> None

let rec extractVariables vs counter =
  if counter >= Array.length vs
  then []
  else (Array.get vs counter)::(extractVariables vs (counter + 1))

let format
    {vars = vs; returnAddress =  _ ; callerCps = _; callerTrailpoint = _} =
  let vList = extractVariables vs 0
  in vList
