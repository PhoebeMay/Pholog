open Logs


let myLog =
  let log = Logs.Src.create "logs"
  in Logs.set_level (Some(Debug));
   Logs.set_reporter (Logs_fmt.reporter ~pp_header:(Fmt.nop) ());
  log


let logDebug msg = Logs.debug ~src:myLog msg

let logError msg = Logs.err ~src:myLog msg
