open Core
open Dt

let rec foldLeft f xs r =
  match xs with [] -> r | y :: ys -> foldLeft f ys (f y r)

let seen var soFar =
  let lookup = Hash_set.exists soFar ~f:(fun f -> f = var) in
  if lookup then true else ( Hash_set.add soFar var ; false )

let getStructMapInt (structMapGen : structMapGen) str =
  let x = Hashtbl.find structMapGen.mapping str in
  match x with
  | Some n -> n
  | None ->
      let next = structMapGen.nextFree in
      Hashtbl.add_exn structMapGen.mapping ~key:str ~data:next ;
      structMapGen.nextFree <- next + 1 ;
      next

let getStructMapFid (structMapGen : structMapGen) (name, nums) =
  (getStructMapInt structMapGen name, nums)

let reverseTbl tbl =
  let newTable = Hashtbl.create ~size:(Hashtbl.length tbl) intImp in
  Hashtbl.iteri tbl ~f:(fun ~key ~data:value ->
      Hashtbl.add_exn newTable ~key:value ~data:key ) ;
  newTable

let reverseStructmap sg = reverseTbl sg.mapping

let rec strInstructionArray counter arr =
  if Array.length arr <= counter then "\n"
  else
    string_of_int counter ^ ". "
    ^ show_instruction arr.(counter)
    ^ "\n"
    ^ strInstructionArray (counter + 1) arr
