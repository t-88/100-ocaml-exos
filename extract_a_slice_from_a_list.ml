let slice xs i k = 
  let rec i_slice xs i = 
    match xs with 
      | [] -> []
      | x :: xs -> if i = 0 then x :: xs else i_slice xs (i-1)
  in
  let rec k_slice xs k =
    match xs with 
    | [] -> []
    | x :: xs -> if k = 1 then [] else x :: k_slice xs (k-1)
  in k_slice (i_slice xs i) k;;
