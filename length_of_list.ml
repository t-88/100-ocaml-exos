let length l = 
  let rec length l c =
    match l with
    | [] -> c
    | x :: l -> length l (c+1)
  in length l 0;;
