module F = Format

(* practice *)
let rec interp (e : Ast.ae) : int =
       match e with
       Ast.Num x -> x
       | Ast.Add (y,z) -> ((interp y) + (interp z))
       | Ast.Sub (q,w) -> ((interp q) - (interp w))
