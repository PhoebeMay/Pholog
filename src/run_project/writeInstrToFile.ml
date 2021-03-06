open Dt
open GenCode
open Utils
open Logging
open TypeChecker


let writeInstructions location s tc=
  let lexbuf = Lexing.from_string s
  in let parseTree = Parser.main Lexer.token lexbuf
  in let _typeinfo = typeCheck tc parseTree
  in let instr  = genCode parseTree
  in let {code=flatcode; nums=_arrLens; structMap = _lookupmap} = instr
  in let () = logInfo (fun m -> m "%s" (strInstructionArray 0 flatcode))
  in let toWrite = instr
  in let _ = Sexplib.Sexp.save_mach location (sexp_of_writtenInstr toWrite)
  in ()
