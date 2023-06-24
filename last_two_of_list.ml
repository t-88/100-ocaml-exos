let rec last_two  = function
  | [] | [_]  -> None
  | [x;y]     -> Some [x;y]  
  | _ :: l      -> last_two l;;  