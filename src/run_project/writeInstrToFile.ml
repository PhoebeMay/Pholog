open Dt
open GenCode
open Utils
open Logging
open TypeChecker


let writeInstructions location s tc=
  let lexbuf = Lexing.from_string s
  in let parseTree = Parser.main Lexer.token lexbuf
(* in let functT,query = genFunctionTable parseTree *)

  in let _typeinfo = typeCheck tc parseTree
  in let instr  = genCode parseTree
  in let {code=flatcode; nums=arrLens; structMap = lookupmap} = instr

  in let () = logInfo (fun m -> m "%s" (strInstructionArray 0 flatcode))
  in let toWrite = instr
  (* in let file = Pervasives.open_out_bin "abstractmachine1/instructions" *)
  in let _ = Sexplib.Sexp.save_mach location (sexp_of_writtenInstr toWrite)
  (* in let () = logDebug (fun m -> m "%s" (strInstructionArray 0 flatcode)) *)
  in ()
