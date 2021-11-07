module F = Format 

type unop = Neg
type bnop = Add | Sub | Mul | Div
type exp =
        Constant of int
        | Unary of unop * exp
        | Binary of exp * bnop * exp

let rec eval x =
       match x with 
                Constant a -> a
                | Binary(b ,c,d )-> 
                                (match c with
                                Add ->  (eval b) + (eval d)
                                | Sub -> (eval b) - (eval d)
                                | Mul ->  (eval b) * (eval d)
                                | Div -> (eval b) / (eval d) 
                                )                
                | Unary(Neg, e) -> -(eval e)

