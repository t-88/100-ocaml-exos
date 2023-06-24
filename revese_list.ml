let rec rev l =
  match l with
    | []      -> []
    | x :: [] -> [x]
    | x :: l  ->  (rev l) @ [x];;


let rev_acc l =
  let rec rev l c =
    match l with
      | []      -> c
      | x :: l  ->  rev l (x :: c)    
  in 
    rev l [];;