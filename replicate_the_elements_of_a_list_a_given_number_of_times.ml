let rec replicate xs t = 
  let many x c =
    let rec many x c acc = 
      if c = 0 then acc else many x (c-1) (x :: acc)
    in many x c []
  in 
    match  xs with
      | [] -> []
      | x :: xs -> (many x t) @ replicate xs t;;  