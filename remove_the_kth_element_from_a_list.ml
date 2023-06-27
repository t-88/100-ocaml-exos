let rec remote_at k xs = 
  match xs with
  | [] -> []
  | x :: xs -> if k = 0 then xs else x :: remote_at (k-1) xs;; 