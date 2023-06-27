let rec duplicate xs = 
  match xs with
  | [] -> []
  | x :: xs -> x :: x :: duplicate xs;; 