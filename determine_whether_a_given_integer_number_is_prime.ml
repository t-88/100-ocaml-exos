let is_prime (x) = 
  let rec is_prime i sqx =
    if i < sqx then
      match (x mod i) with
      | 0 -> false
      | _ -> is_prime (i + 1) sqx
    else true
  in is_prime 2 (int_of_float (Float.sqrt (float_of_int x) ) + 1) ;;