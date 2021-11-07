module F = Format

type t = 
  | Int of int
  | Var of string

type state = 
  | S0
  | S1
  | S2
  | S3

let char_to_int c = (int_of_char c) - 48 
let char_to_str c = Char.escaped c

(* lex : char list -> t *)
let lex chars = 
        let rec lex_impl state chars v = 
                match state with
                | S0 ->
                                begin
                                match chars with
                                | h::t ->
                                                begin
                                                match h with
                                               '0' .. '9' -> lex_impl S1 t (v*10 + (char_to_int h))
                                               | '-' -> lex_impl S1 t (-1)
                                               | 'a' .. 'z' -> lex_impl S1 t (char_to_str h)
                                               | _ -> failwith "Not a valid Integer"
                                                end
                                | [] -> failwith "Not a valid Integer"
                                end
                 | S1 -> []


let pp fmt v = 
  match v with
  | Int i -> F.fprintf fmt "Int %d" i
  | Var x -> F.fprintf fmt "Var %s" x 
