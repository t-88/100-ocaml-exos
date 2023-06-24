(* [x]  => one element *)
 
let rec last l = 
  match l with
  | []      -> None
  | x :: [] -> Some x
  | x :: l  -> last l;;  

let x = last [1];;


Utils.print_option x
