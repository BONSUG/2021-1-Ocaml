module F = Format

(* practice *)
let rec interp (s : Store.t) (e : Ast.vae) : int =
       match e with
       Num x -> x
       | Add (y,z) -> (interp s y) + (interp s z)
       | Sub (q,w) -> (interp s q) - (interp s w)
       | Id i -> Store.find i s
       | LetIn (n,m,l) ->interp (Store.insert n (interp s m) s) l
