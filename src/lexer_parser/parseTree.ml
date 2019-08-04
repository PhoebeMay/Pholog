open Core

type var = string [@@deriving show, sexp]

type funct = string [@@deriving show, sexp]

type pred = string [@@deriving show, sexp]

type 'a term = TVar of 'a | TFun of funct * 'a term list | TInt of int
[@@deriving show, sexp]

type 'a atom = Atom of pred * 'a term list [@@deriving show, sexp]

type 'a arithBase = Var of 'a | Int of int [@@deriving show, sexp]

type 'a mathExpr =
  | Plus of 'a mathExpr * 'a mathExpr
  | Subtract of 'a mathExpr * 'a mathExpr
  | Base of 'a arithBase
[@@deriving show, sexp]

type 'a isExpr = IsExpr of 'a * 'a mathExpr [@@deriving show, sexp]

type 'a clauseBodyVal = CAT of 'a atom | CAR of 'a isExpr | Cut | Fail
[@@deriving show, sexp]

type 'a clause = Clause of 'a atom * 'a clauseBodyVal list
[@@deriving show, sexp]

type 'a typeins =
  | IntTyp
  | TypeVar of 'a
  | TypeCons of string * 'a typeins list
[@@deriving show, sexp]

type 'a typeDefRight = TypeDefRight of string * 'a typeins list
[@@deriving show, sexp]

type 'a typedef = TypeDef of string * var list * 'a typeDefRight list
[@@deriving show, sexp]

type 'a preddef = PredDef of string * 'a typeins list [@@deriving show, sexp]

type 'a sentence_item = C of 'a clause | D of 'a typedef | P of 'a preddef
[@@deriving show, sexp]

type 'a sentence = Sentence of 'a sentence_item list [@@deriving show, sexp]

type 'a resolvant = Resolvant of 'a clauseBodyVal list
[@@deriving show, sexp]

type 'a program = Program of 'a sentence * 'a resolvant
[@@deriving show, sexp]

type answer = Fail | Ans of string resolvant [@@deriving show]

let addClause (Sentence cs) clause = Sentence (clause :: cs)
