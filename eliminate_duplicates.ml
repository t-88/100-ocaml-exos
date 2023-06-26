let delete xs x  =
  let rec delete xs c x  =
    match xs with
      | [] -> c
      | a :: xs -> if a = x then delete xs c x else delete xs (a :: c) x 
    in delete xs [] x;;




let compress xs = 
  let rec compress xs c =
    match xs with
    | [] -> c
    | x :: xs -> compress (delete xs x) (x :: c)
  in compress xs [];;