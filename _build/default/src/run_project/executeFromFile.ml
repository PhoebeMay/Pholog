open Core
open Dt
open RunProgram
open Logging
open RuntimeDataStructures
open FlattenInstrForPrint

let getStructMap {nums= _; code= _; structMap= sm} = sm

type resFinal = variableFlat list [@@deriving show]

let runFile loc =
  (* let () = logError (fun m -> m "run file") *)
  (* in  *)
  let loadedCode = Sexp.load_sexp loc |> writtenInstr_of_sexp in
  let r = runProgram loadedCode in
  match r with
  | Some res ->
      let ans =
        List.map ~f:(flattenSv loadedCode.structMap) (Array.to_list res)
      in
      let () = logInfo (fun m -> m "Result  h: %a " pp_resFinal ans) in
      ()
  | None -> print_endline "failure"
