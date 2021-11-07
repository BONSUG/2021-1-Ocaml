module F = Format

(* practice & homework *)
let rec interp_e (s : Store.t) (e : Ast.expr) : Store.value = 
        match e with
          | Num x -> NumV x
          | Add (y,z) -> let a = (interp_e s y) in
                         let b = (interp_e s z) in
                         (match a,b with
                         NumV a , NumV b ->NumV (a+b)
                        |_->  failwith (Format.asprintf "Invalid addition: %a + %a" Ast.pp_e y Ast.pp_e z)
                         )
          | Sub (q,w) -> let a = (interp_e s q) in
                         let b = (interp_e s w) in
                         (match a,b with
                         NumV a , NumV b ->NumV (a-b)
                        |_->  failwith (Format.asprintf "Invalid subtraction : %a - %a" Ast.pp_e q Ast.pp_e w)
                         )
          | Id r -> Store.find r s
          | LetIn (j,k,l) -> interp_e (Store.insert j (interp_e s k) s) l
          | Fun (a,b) -> ClosureV(a,b,s)
          | App (c,d) -> 
                          let x = interp_e s c in
                          let y = interp_e s d in
                          match x with
                          ClosureV (a1,b1,s1)  -> interp_e  (Store.insert a1 y s1) b1
                          | _-> failwith  (Format.asprintf "Not a function : %a" Ast.pp_e c)


(* practice & homework *)
let interp (p : Ast.fvae) : Store.value = 
        match p with
        Prog x -> interp_e [] x
