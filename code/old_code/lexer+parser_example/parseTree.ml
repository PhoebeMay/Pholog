type var = string
type funct = string
type pred = string
type term = TVar of var | TFun of var * term list
type atom = Atom of pred * term list
type clause = Clause of atom * atom list
type sent = clause list
type resolvant = atom list
type prog = sent * resolvant



(*
let rec printTree = function
  | Int(x) -> Printf.printf "%d" x; ()
  | Plus(x,y) -> Printf.printf "Plus(";
      printTree x;
      Printf.printf ",";
      printTree y;
      Printf.printf ")"
  | Minus(x,y) -> Printf.printf "Minus(";
      printTree x;
      Printf.printf ",";
      printTree y;
      Printf.printf ")"
*)
