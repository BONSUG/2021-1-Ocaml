module F = Format

let rec interp_e (fenv : FEnv.t) (s : Store.t) (e : Ast.expr) : int = 
	(* write your code *)
        match e with
          | Num x -> x
          | Add (y,z) -> (interp_e fenv s y) + (interp_e fenv s z)
          | Sub (q,w) -> (interp_e fenv s q) - (interp_e fenv s w)
          | Id r -> Store.find r s
          | LetIn (j,k,l) -> interp_e fenv (Store.insert j (interp_e fenv s k) s) l
          | FCall(n,m) -> 
                          let (u,i) = FEnv.find n fenv in
                          let rec call m u acc=
                                  match m,u with
                                  h::t ,uh::ut -> 
                                          let ns = Store.insert uh (interp_e fenv s h) acc in
                                          call t ut ns
                                  | [],[]-> acc
                                  | _, _ -> failwith "Arity mismatched"
                          in
                          interp_e fenv (call m u s) i
    

let interp_d (fenv : FEnv.t) (fd : Ast.fundef) : FEnv.t = 
  (* write your code *)
        match fd with
        FDef (x,y,z) -> FEnv.insert x y z fenv 


(* practice *)
let interp (p : Ast.f1vae) : int =
        let rec interp2 p acc =
                match p with
                x -> (
                                match x with
                                h::t -> interp2 t (interp_d acc h)
                                |[] -> acc
                                )
        in
        match p with
        Prog(x,y) -> interp_e (interp2 x []) [] y 
