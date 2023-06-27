let rotate xs k = 
  let rec  rotate xs k acc = 
    match xs with
        | [] -> (acc , xs)
        | x :: xs -> if k = 1 then (x :: acc , xs)  else  rotate xs (k-1) (x :: acc) 
  in let (a,b) = rotate xs k []  in b @ (List.rev a);;
