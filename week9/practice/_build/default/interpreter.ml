module F = Format

let rec interp_e (fenv : FEnv.t) (s : Store.t) (e : Ast.expr) : int = 
	(* write your code *)
        match e with
          | Num x -> x
          | Add (y,z) -> (interp_e fenv s y) + (interp_e fenv s z)
          | Sub (q,w) -> (interp_e fenv s q) - (interp_e fenv s w)
          | Id r -> Store.find r s
          | LetIn (j,k,l) -> interp_e fenv (Store.insert j (interp_e fenv s k) s) l
          | FCall (n,m) -> (
                let (u,i) = FEnv.find n fenv in
                interp_e fenv s (LetIn(u,m,i))
          )


let interp_d (fd : Ast.fundef) : FEnv.t = 
	(* write your code *)
        match fd with
        FDef (x,y,z) -> FEnv.insert x y z FEnv.empty


(* practice *)
let interp (p : Ast.f1vae) : int = 
	(* write your code *)
        match p with
        | Prog(x,y) -> interp_e (interp_d x) [] y
