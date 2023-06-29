


exception OutOfRange of string;;
let rec rand_select (xs: string list) (y : int) : string list = 
  let rec get_idx (xs : string list) (i: int) : string = 
    match xs with
    | [] -> raise (OutOfRange "empty list cant be searched")
    | x :: xs -> if i = 0 then x else get_idx xs (i-1) 
  in
  match xs with 
  | [] -> []
  | x :: xs -> if y = 0 then [] else (get_idx (x::xs) (Random.int ((List.length xs) + 1))) :: rand_select (x::xs) (y-1);;