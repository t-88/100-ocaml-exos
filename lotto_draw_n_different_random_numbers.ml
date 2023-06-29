

let range i k = 
  let rec range i k c = 
    if i <= k then range (i+1) k (i :: c) else  c 
  in if k > i then List.rev(range i k []) else range k i [] ;; 

exception OutOfRange of string;;
let rec rand_select xs y = 
  let rec get_idx xs  i = 
    match xs with
    | [] -> raise (OutOfRange "empty list cant be searched")
    | x :: xs -> if i = 0 then x else get_idx xs (i-1) 
  in
  match xs with 
  | [] -> []
  | x :: xs -> if y = 0 then [] else (get_idx (x::xs) (Random.int ((List.length xs) + 1))) :: rand_select (x::xs) (y-1);;


let lotto_select i j =  rand_select (range i j) (Random.int 10);; 
