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
        |S0 ->
                begin
                        match chars with
                        | h::t ->
                                        begin
                                                match h with
                                                '0' .. '9' -> lex_impl S1 t (v*10 + (char_to_int h))
                                                | '-' -> lex_impl S1 t (-1)
                                                | _ -> failwith "Not a valid Integer"
                                        end
                        |[] -> failwith "Not a valid Integer"
                end
       |S1 ->
                begin
                        match chars with
                        | h::t ->
                                        begin
                                                match h with
                                                |'0' .. '9' ->(
                                                                match v with
                                                                Var x -> lex_impl S2 t (x^(char_to_str h))
                                                                | Int y -> (
                                                                         if y=-1 then lex_impl S2 t (y*(char_to_int h))
                                                                         else lex_impl S2 t (y*10 + (char_to_int h))
                                                                             )
                                                                | _ -> failwith "Not a valid integer or a valid variable"
                                                )
                                                | _ -> failwith "Not a valid Integer"
                                        end
                        |[] -> lex_impl S2 chars v
                end
        |S2 ->
                begin
                        match chars with
                        | h::t ->
                                        begin
                                                match h with
                                                |'0' .. '9' ->(
                                                                match v with
                                                                Var x -> lex_impl S3 t (x^(char_to_str h))
                                                                | Int y -> (
                                                                        if y<0 then lex_impl S3 t (y*10 + -(char_to_int h))
                                                                        else lex_impl S3 t (y*10 + (char_to_int h))
                                                                         )
                                                                             )
                                                | 'a' .. 'z' | 'A' .. 'Z'|'_' | '\''-> lex_impl S3 t (v^(char_to_str h))                     
                                                | _ -> failwith "Not a valid  integer or a valid variable"
                                        end
                        |[] -> lex_impl S3 chars v
                end
        |S3 ->
                begin
                        match chars with
                        | h::t ->
                                 begin
                                         match h with
                                         |'0' .. '9' ->(
                                                 match v with 
                                                 Var x-> lex_impl S3 t (x^(char_to_str h))
                                                | Int y-> (
                                                         if y<0 then lex_impl S3 t (y*10 + -(char_to_int h))
                                                        else lex_impl S3 t (y*10 + (char_to_int h)) 
                                                          )
                                                       )
                                                | 'a' .. 'z' | 'A' .. 'Z'|'_'| '\'' -> lex_impl S3 t (v^(char_to_str h))
                                                | _ -> failwith "Not a valid Integer"
                                 end
                        |[] -> (
                                match v with 
                                Var x -> Var x
                                |Int y -> Int y                
                                                )
                             
                end
        in

        lex_impl S0 chars 0

let pp fmt v = 
  match v with
  | Int i -> F.fprintf fmt "Int %d" i
  | Var x -> F.fprintf fmt "Var %s" x 
