type 'a node =
  | One of 'a 
  | Many of 'a node list


let rec flatten = function
  | [] -> []
  | One x :: xs -> [x] @ flatten xs
  | Many x :: xs -> flatten x @ flatten xs

let flatten_acc l =
  let rec flatten l c = 
    match l with
      | [] -> c
      | One x :: xs -> flatten xs (c @ [x])
      | Many x :: xs -> flatten xs (flatten x c)
  in flatten l [] 
