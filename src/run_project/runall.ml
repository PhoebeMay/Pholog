open Core
open RunProgram
open GenCode
open Logging
open FlattenInstrForPrint
open TypeChecker
open Dt
open RuntimeDataStructures


let execute s tc =
  let lexbuf = Lexing.from_string s
  in let parseTree = Parser.main Lexer.token lexbuf

  (* in let () = logDebug (fun m -> m "Parse tree:\n %a" pp_parsetree parseTree) *)
  (* in let str_parsetree = Sexp.to_string (sexp_of_program sexp_of_string parseTree) *)
  (* in let () = print_endline str_parsetree *)

  in let _typeinfo = typeCheck tc parseTree



  (* in let str_functT =   Sexp.to_string ( sexp_of_functTlist (Hashtbl.to_alist functT))
     in let () = print_endline str_functT

     in let str_query = Sexp.to_string (sexp_of_query query)
     in let () = print_endline str_query *)

  in let instr  = genCode parseTree
  in let {code=_flatcode; nums=_arrLens; structMap = lookupmap} = instr

  (* in let str_code = Sexp.to_string(sexp_of_code (code,nums,st))
     in let () = print_endline str_code  *)

  (* in let flatcode = lineariseCode code *)
  (* in let () = logDebug (fun m -> m "%s" (strInstructionArray 0 flatcode)) *)

  (* in let instr = {code=flatcode; nums=nums; structMap = st} *)

  in let str_inst = Sexp.to_string(sexp_of_writtenInstr instr)
  in let () = logDebug (fun m -> m "%s" str_inst)

  in let res = runProgram instr

  (* in let str_res = Sexp.to_string(sexp_of_result res) *)
  (* in let () = print_endline str_res *)

  in let ans = match res with
        Some(res) ->
        let str_res = Sexp.to_string(sexp_of_result res)
        in let () = logInfo (fun m -> m "Result: %s" str_res)
        in
        Some(List.map ~f:(flattenSv lookupmap) ( Array.to_list res))
      | None ->
        print_endline "fail";
        None
  in ans
