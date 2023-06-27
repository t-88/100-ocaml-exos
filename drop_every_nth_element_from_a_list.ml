let drop xs t = 
  let rec drop xs t c = 
    match xs with
    | [] -> []
    | x :: xs -> if c = 1 then drop xs t t else x :: drop xs t (c-1)
  in drop xs t t;;