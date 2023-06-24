exception Err of string;;

let rec nth l c =
  match l with
  | [] -> raise(Err "out of range")
  | x :: l -> if c = 0 then Some x else nth l (c-1);;
