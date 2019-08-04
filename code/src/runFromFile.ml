open Runall
open Logging
open WriteInstrToFile
open ExecuteFromFile
open Dt





let addr = Sys.argv.(1)
(* let () = print_endline addr *)
let () = runFile ("bcode/" ^ addr)
(* let _ = logError (fun m -> m "finish run") *)
