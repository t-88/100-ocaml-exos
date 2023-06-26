let rec pack_x xs y =
  match xs with
  | [] -> []
  | x :: xs -> if x = y then x :: pack_x xs y else [];;


let delete xs x  =
  let rec delete xs c x  =
    match xs with
      | [] -> c
      | a :: xs -> if a = x then delete xs c x else delete xs (a :: c) x 
    in delete xs [] x;;


let rec pack xs = 
  match xs with
  | [] -> []
  | x :: xs -> pack_x (x :: xs) x :: pack (delete xs x);;  
