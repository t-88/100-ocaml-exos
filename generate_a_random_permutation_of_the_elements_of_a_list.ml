exception EmptyList of string;;

let some_permutation xs = 
  let rec permutation xs acc  =
    match xs with
    | [] -> acc
    | x :: xs ->   permutation xs (if (Random.int 11) <= 5 then x :: acc else acc @ [x])  
  in permutation xs [];;  



let permutation xs = 
  let rec pop_i xs i = 
    match xs with 
    | [] -> raise(EmptyList "cant pop from empty list")
    | x :: xs -> if i <= 0 then x,xs else pop_i (xs @ [x]) (i - 1)
  in  
  let rec permutation xs acc len  = 
    match xs with
    | [] -> acc
    | x :: xs ->  let z,y =  (pop_i (x::xs) (Random.int len-1)) in permutation  y (z::acc) (len-1)
  in permutation xs [] (List.length xs) ;;  

    
  