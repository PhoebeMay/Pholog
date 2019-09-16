open Dt
open ParseTree
open Core


type functTval = (location ParseTree.clause * intnum) list [@@deriving sexp]
type query = location ParseTree.clauseBodyVal list * intnum [@@deriving sexp]
type code = (functionId * (int * instruction sexp_list) sexp_list) sexp_list * Dt.arrLens * Dt.structureLookup [@@deriving sexp]
type functTlist = (functionId * functTval) list [@@deriving sexp ]

val genCode : funct program -> writtenInstr
