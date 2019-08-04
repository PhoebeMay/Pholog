type var = string [@@deriving show]
type funct = string [@@deriving show]
type pred = string [@@deriving show]

type 'a term = TVar of 'a | TFun of funct * 'a term list [@@deriving show]
type 'a atom = Atom of pred * 'a term list [@@deriving show]
type 'a clause = Clause of 'a atom * 'a atom list [@@deriving show]
type 'a sentence = Sentence of 'a clause list [@@deriving show]
type 'a resolvant = Resolvant of 'a atom list [@@deriving show]
type 'a program = Program of 'a sentence * 'a resolvant [@@deriving show]
type answer = Fail | Ans of string resolvant [@@deriving show]

let addClause (Sentence(cs)) clause = Sentence(clause::cs)

let () =
  let a = show_program pp_var (Program(Sentence([]),Resolvant([])))
  in print_endline a

let rec strAll f = function
  | [] -> ""
  | [x] -> f x
  | x::xs -> (f x) ^ ";" ^ (strAll f xs)

let rec strTerm = function
  | TVar(v) -> "TVar(\"" ^ v ^ "\")"
  | TFun(v,ts) -> "TFun(\"" ^ v ^ "\",[" ^ (strAll strTerm ts) ^ "])"

let strAtom = function
  | Atom(p,ts) -> "Atom(\"" ^ p ^ "\",[" ^ (strAll strTerm ts) ^ "])"

let strClause = function
  | Clause(at, ats) -> "Clause(" ^ strAtom at ^ ",[" ^ (strAll strAtom ats) ^ "])\n"

let strSentence = function
  | Sentence([]) -> "Sentence([])"
  | Sentence(cs) -> "Sentence([" ^ (strAll strClause cs) ^ "])"

let strResolvant = function
  | Resolvant([]) -> "Resolvant([])"
  | Resolvant(ats) -> "Resolvant([" ^ (strAll strAtom ats) ^ "])"

let strAns = function
  | Ans(r) -> "Ans(" ^ strResolvant r ^ ")"
  | Fail -> "False"

let strProgram = function
  | Program(sent,res) ->  "Program(" ^ (strSentence sent) ^ "," ^ (strResolvant res) ^ ")"

let printProgram p = Printf.printf "%s" (strProgram p)
