type 'a rle = 
  | One of 'a
  | Many of int * 'a;;
  
let count xs y =
  let rec count xs c =  
    match xs with 
      | [] -> c
      | x :: xs -> if x = y then count xs (c + 1) else c
  in count xs 0;; 


let rec step xs y = 
  if y = 0 then xs
  else 
    match xs with
      | [] -> []
      | _ :: xs -> step xs (y-1);;

let rev_acc l =
  let rec rev l c =
    match l with
      | []      -> c
      | x :: l  ->  rev l (x :: c)    
  in 
    rev l [];;  
let encode xs = 
  let rec encode xs c =
    match xs with
    | [] -> rev_acc c
    | x :: xs -> match (count xs x) with 
                 | 0 ->      encode xs ((One x) :: c) 
                 | y -> encode (step xs y) ((Many (y,x)) :: c)
  in encode xs [];;