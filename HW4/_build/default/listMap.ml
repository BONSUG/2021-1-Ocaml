module F = Format

type t = (string * string) list 

let empty = []

let add key value map = ((key,value)::map)

let rec find key map = 
        (* write your code *)
        match map with
        (x,y)::t -> if key=x then y
                    else (find key t)
        | _ -> failwith "No such key exists"

let rec erase key map = 
        (* write your code *)
        match map with
        (x,y)::t -> 
                if key=x then t
                else (x,y)::(erase key t)                
        | _ -> failwith "No such key exists"

let print_map fmt map = 
  let rec print_map_impl map = 
    match map with
    | [] -> ()
    | (k, v) :: t -> 
        let () = F.fprintf fmt "(%s, %s) " k v in
        print_map_impl t
  in
  let () = F.fprintf fmt "[ " in
  let () = print_map_impl map in
  F.fprintf fmt "]"
