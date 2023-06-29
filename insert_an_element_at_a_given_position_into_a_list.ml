let rec insert_at y i xs = 
  match xs with
  | [] -> [y]
  | x :: xs -> if i = 0 then y :: x  :: xs else x:: insert_at y (i-1) xs;;