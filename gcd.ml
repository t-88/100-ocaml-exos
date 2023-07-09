let pwr x y =
  let rec pwr a x y =  
    if y <= 0 then x 
    else pwr a (x*a) (y-1)
  in pwr x 1 y ;;

let factors x = 
  let rec factors x sqrx acc i  =
      if x == 1 || i > sqrx  then 
        if (List.length acc) == 0 then [x] else acc
      else match x mod i with
      | 0 -> factors (x / i) sqrx (i::acc) i  
      | _ -> factors x sqrx acc (i+1)
    in factors x (int_of_float (Float.sqrt (float_of_int x)) +  1) [] 2 ;;

let count_and_drop xs y = 
  let rec count_and_drop xs y acc = 
  match xs with
  | []    -> ([],acc)
  | x::xs -> if x = y  then count_and_drop xs y (acc+1) 
             else  let (l , a) = count_and_drop xs y acc in (x::l, a)
  in count_and_drop xs y 0;;

let gcd x y =
  let rec commons a b acc = 
    if ((List.length a) = 0) || ((List.length b) = 0) then acc
    else
      match a with 
      | [] -> acc
      | x::a ->      let (_a,i) = (count_and_drop (x::a) x) 
                  in let (_b,j) = (count_and_drop b x) 
                  in  if i < j then commons _a _b (acc * (pwr x i )) 
                      else  commons _a _b (acc * (pwr x j ))
  in commons (factors x) (factors y) 1;;



let rec euclid_gcd a b = 
  if b = 0 then a else euclid_gcd b (a mod b);;