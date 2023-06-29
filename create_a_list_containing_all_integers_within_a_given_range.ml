let range i k = 
  let rec range i k c = 
    if i <= k then range (i+1) k (i :: c) else  c 
  in if k > i then List.rev(range i k []) else range k i [] ;; 