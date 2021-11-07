module F = Format

type t = (string * (string list * Ast.expr)) list

let empty = []

let insert x plist body s = (* write your code *)
        (x,(plist ,body))::s
let rec find x s = 
  (* write your code *)
        match s with
        (a,(b,c))::t -> if x = a then (b,c)
        else find x t
        | _ -> failwith ("Free idenfier " ^ x )
  
let pp fmt s = 
  let rec pp_impl fmt s = 
    match s with
    | [] -> F.fprintf fmt "]"
    | (x, (p, e)) :: t -> F.fprintf fmt "(%s, (%s, %a)) %a" x p Ast.pp_e e pp_impl t
  in
  F.fprintf fmt "[ %a" pp_impl s
