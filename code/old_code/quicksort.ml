open Printf

let rec quicksort = function
        | [] -> []
        | x::xs -> let smaller, larger = List.partition (fun y -> y < x ) xs
               in quicksort smaller @ (x::quicksort larger)

let res = (quicksort [2;1;3] )

let () = List.iter(printf "%d") res ;; printf "\n" 
