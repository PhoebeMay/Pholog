open Core

type 'a node = {mutable prev: 'a node option; value: 'a} [@@deriving show]

let createEmpty = None

let createNew v ~prev:p = Some {prev= p; value= v}

let rec getPredsh node =
  match node with None -> [] | Some n -> n :: getPredsh n.prev

let getPreds node = match node with None -> [] | Some n -> getPredsh n.prev

let rec getLength node =
  match node with None -> 0 | Some n -> 1 + getLength n.prev
