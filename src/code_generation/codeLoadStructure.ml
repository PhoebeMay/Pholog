open Dt
open Utils
open ParseTree
open Core

type abstractInstruction2 =
  | Abs_SetIntE of int
  | Abs_SetVE of location
  | Abs_PutStructureT of structure * temp
  | Abs_SetValue of temp

type putArg = {
  build : abstractInstruction2 list;
  load : abstractInstruction2;
}

let rec loadFunBody seenSoFar nextFreeTemp structMapGen term =
  match term with
  | TInt n -> { build = []; load = Abs_SetIntE n }
  | TVar var -> { build = []; load = Abs_SetVE var }
  | TFun (f, ts) ->
      let nextFree = Temp !nextFreeTemp in
      let _ = nextFreeTemp := !nextFreeTemp + 1 in
      let fid = (f, List.length ts) in
      let build, load =
        loadAllFunBodyAbs ts seenSoFar
             nextFreeTemp structMapGen
      in
      {
        build =
          build
          @ [ Abs_PutStructureT (getStructMapFid structMapGen fid, nextFree) ]
          @ load;
        load = Abs_SetValue nextFree;
      }

and loadAllFunBodyAbs terms seenSoFar nextFreeTemp
    (structMapGen : structMapGen) =
  let collectBuild (xs : putArg list) =
    List.map ~f:(fun { build = b; load = _ } -> b) (xs : putArg list)
  in
  let collectLoad xs = List.map ~f:(fun { build = _; load = l } -> l) xs in
  let res =
    List.map ~f:(loadFunBody seenSoFar nextFreeTemp structMapGen) terms
  in
  (List.concat (collectBuild res), collectLoad res)

let deAbst seenSoFar absinstr =
  match absinstr with
  | Abs_SetIntE n -> SetInt n
  | Abs_SetVE v -> if seen v seenSoFar then SetValue v else SetVariable v
  | Abs_PutStructureT (x, y) -> PutStructureT (x, y)
  | Abs_SetValue t -> SetValue (T t)

let loadAllFunBody terms seenSoFar nextFreeTemp structMapGen =
  let abs1, abs2 =
    loadAllFunBodyAbs terms seenSoFar nextFreeTemp structMapGen
  in
  let ans1 = List.map ~f:(deAbst seenSoFar) abs1 in
  let ans2 = List.map ~f:(deAbst seenSoFar) abs2 in
  (ans1, ans2)
