open Format

let print_option x = 
  match x with
    | None -> printf "no last\n"
    | Some x -> printf "%d\n" x;;
    
