open Runall
open Logging
open WriteInstrToFile
open ExecuteFromFile
open Dt





let addr = Sys.argv.(1)
let () = runFile ("bcode/" ^ addr)
