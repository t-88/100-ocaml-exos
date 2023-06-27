type 'a rle = 
  | One of 'a
  | Many of int * 'a;;
  


let decode xs = 
  let rec spread c z x = 
    match z with
    | 0 -> c
    | _ -> spread (x::c) (z-1) x
  in
  let rec decode xs c = 
    match xs with
    | [] -> List.rev c
    | y :: xs -> match y with
                 | One x -> decode xs (x :: c)
                 | Many (z,x) -> decode xs (spread c z x)
  in decode xs [];;