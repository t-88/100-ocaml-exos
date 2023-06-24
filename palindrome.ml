let length l = 
  let rec length l c =
    match l with
    | [] -> c
    | x :: l -> length l (c+1)
  in length l 0;;

exception Err of string;;
let rec nth l c =
  match l with
  | [] -> 0
  | x :: l -> if c = 0 then x else nth l (c-1);;
      
  
  

let is_palindrome l =
  let rec is_palindrome l a b =
    if a >= b  then true 
    else if (nth l a) = (nth l b) then is_palindrome l (a+1) (b-1) 
        else false  
  in is_palindrome l 0 (length(l) - 1);; 


let rev_acc l =
  let rec rev l c =
    match l with
      | []      -> c
      | x :: l  ->  rev l (x :: c)    
  in 
    rev l [];;

let is_palindrome_rev l =
  l = (rev_acc l);;
  