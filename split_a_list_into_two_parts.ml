let split xs c = 
  let rec split xs c acc =
    match xs with 
    | [] -> (List.rev acc,[])
    | x :: xs -> if c = 0 then (List.rev acc,x :: xs) else split xs (c-1) (x :: acc)
  in split xs c [];;  
