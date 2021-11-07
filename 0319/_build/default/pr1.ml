module F = Format

type unary_number =
        Z
        | S of unary_number

let rec add n m =
        match n with
        Z -> m
        |S (x) -> S (add x m)
        
let rec mul n m acc =
        match n with
        Z -> acc
        |S (x) -> mul x m (add acc m) 
let rec print fmt u=
        match u with
        S Z -> F.fprintf fmt "S Z"
        |S (x)-> F.fprintf fmt "S(%a)" print x
        | _ -> failwith "error"

let _ =
        let a = add (S(S(S(S(S Z))))) (S Z) in
        let _ =F.printf "%a\n" print a in
        let b = mul (S(S Z)) (S(S Z)) Z in
        F.printf "%a\n" print b

