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
                                               '0' .. '9' -> lex_impl S1 t (Int (char_to_int h))
                                               | '-' -> lex_impl S1 t (Int (-1))
                                               |'a' .. 'z' -> lex_impl S1 t (Var (char_to_str h))
                                               | _ -> failwith "Not a valid integer or a valid variable"
                                                end
                                | [] -> failwith "Not a valid integer or a valid variable"
                                end
                 | S1 -> 
                                begin
                                match v with
                                Int x ->
                                        begin
                                                match chars with
                                                h::t ->
                                                        (
                                                               match h with
                                                               '0' .. '9' -> (
                                                                       if x<0 then lex_impl S2 t (Int (x*(char_to_int h)))
                                                                       else lex_impl S2 t (Int (x*10 +  (char_to_int h))))
                                                               | _ -> failwith "Not a valid integer or a valid variable"
                                                        )
                                                | [] -> lex_impl S3 chars v
                                        end
                                |Var y ->
                                                begin
                                                        match chars with
                                                        h::t -> 
                                                                (
                                                                 match h with
                                                                'a' .. 'z' | 'A' .. 'Z'|'_' |'\''|'0' .. '9' -> lex_impl S2 t (Var (y^(char_to_str h)))
                                                                | _ -> failwith "Not a valid integer or a valid variable"
                                                       
                                                        )
                                                        | [] -> lex_impl S3 chars v
                                                end
                                end
                                                               
                                        
                                
                                                                                                                                                                                                                                                                                                     
                 | S2 -> 
                                begin
                                match v with
                                Int x ->
                                        begin
                                                match chars with
                                                h::t ->
                                                        (
                                                               match h with
                                                               '0' .. '9' -> (
                                                                       if x<0 then lex_impl S3 t (Int (x*10 + (-(char_to_int h))))
                                                                       else lex_impl S3 t (Int (x*10 +  (char_to_int h))))
                                                               | _ -> failwith "Not a valid integer or a valid variable"
                                                        )
                                                | [] -> lex_impl S3 chars v
                                        end
                                |Var y ->
                                                begin
                                                        match chars with
                                                        h::t -> 
                                                                (
                                                                 match h with
                                                                'a' .. 'z' | 'A' .. 'Z'|'_' | '\'' |'0' .. '9' -> lex_impl S3 t (Var (y^(char_to_str h)))
                                                                | _ -> failwith "Not a valid integer or a valid variable"
                                                       
                                                        )
                                                        | [] -> lex_impl S3 chars v
                                                end
                                end

                 | S3 -> 
                                begin
                                match v with
                                Int x ->
                                        begin
                                                match chars with
                                                h::t ->
                                                        (
                                                               match h with
                                                               '0' .. '9' -> (
                                                                       if x<0 then lex_impl S3 t (Int (x*10 + (-(char_to_int h))))
                                                                       else lex_impl S3 t (Int (x*10 +  (char_to_int h))))
                                                               | _ -> failwith "Not a valid integer or a valid variable"
                                                        )
                                                        | [] ->Int x
                                               		
                                        end
                                |Var y ->
                                                begin
                                                        match chars with
                                                        h::t -> 
                                                                (
                                                                 match h with
                                                                'a' .. 'z' | 'A' .. 'Z'|'_' |'\''|'0' .. '9' -> lex_impl S3 t (Var (y^(char_to_str h)))
                                                                | _ -> failwith "Not a valid integer or a valid variable"
                                                       
                                                        )
                                                        | [] ->Var y

                                                end
                                end
        in
        lex_impl S0 chars (Int 0)


let pp fmt v = 
  match v with
  | Int i -> F.fprintf fmt "Int %d" i
  | Var x -> F.fprintf fmt "Var %s" x 
