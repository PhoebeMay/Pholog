exception OhNo of string

let rec foldLeft f xs r =
  match xs with
  | [] -> r
  | y::ys -> foldLeft f ys (f y r)

let rec select ops soFar totalScore rand = match ops with
  | (title, score)::more ->
      let border = (float_of_int (score + soFar)) /. (float_of_int totalScore) in
      if border >= rand
      then  title
      else select more (soFar + score) totalScore rand
  | [] -> raise(OhNo("not normalised"))

let tasks = [
  ("project dissertation plan", 7);
  ("prin comm supervision work", 1);
  ("bio informatics notes", 1);
  ("ele tripos", 1);
  ("hoare logic tripos",1);
  ("prolog tripos",1);
  ("prin comm notes",1);
]
let rand  =  Random.self_init (); Random.float 1.0
let total = foldLeft (fun (_,b) y -> b + y) tasks 0
let result = select tasks 0 total rand

let () = Printf.printf "You are gonna do %s \n" result
