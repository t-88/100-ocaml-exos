type 'a rle = 
  | One of 'a
  | Many of int * 'a;;
  

let encode xs =
  let to_rle c x = 
    match c with
    | 0 -> One x
    | y -> Many (y+1,x)
  in 
  let rec encode t c acc =
    match t with
    | [] -> []
    | [x] -> (to_rle c x) :: acc
    | a :: (b :: _ as t) -> if a <> b then encode t 0 ((to_rle c a) :: acc)
                         else encode t (c+1) acc
  in List.rev (encode xs 0 []);;