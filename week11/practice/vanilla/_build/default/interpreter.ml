module F = Format

(* practice & homework *)
let rec interp_e (s : Store.t) (e : Ast.expr) : Store.value = 
  (* write your code *)
        match e with
          | Bool b ->BoolV b
          | Num n -> NumV n
          | Add (a1,b1) ->let a = (interp_e s a1) in
                         let b = (interp_e s b1) in
                         (match a,b with
                         NumV a , NumV b ->NumV (a+b)
                        |_->  failwith (Format.asprintf "Invalid addition: %a + %a" Ast.pp_e a1 Ast.pp_e b1)
                         )
          | Sub (c,d)-> let a = (interp_e s c) in
                         let b = (interp_e s d) in
                         (match a,b with
                         NumV a , NumV b ->NumV (a-b)
                        |_->  failwith (Format.asprintf "Invalid subtraction : %a - %a" Ast.pp_e c Ast.pp_e d)
                         )
          | Id i -> Store.find i s
          | LetIn (j,k,l) ->  interp_e (Store.insert j (interp_e s k) s) l
          | App (q,w) ->
                          begin 
                          let x = interp_e s q in
                          let y = interp_e s w in
                          match x with
                          ClosureV (a1,b1,s1)  -> interp_e  (Store.insert a1 y s1) b1
                          | _-> failwith  (Format.asprintf "Not a function : %a" Ast.pp_e q)
                          end
          | Fun (x,y) -> ClosureV(x,y,s)
          | Cond (f,g,h) -> begin
                             let res = interp_e s f in
                             match res with
                             BoolV x -> 
                                     if x=true then (interp_e s g)
                                     else (interp_e s h)
                                            
                              | _ -> failwith (Format.asprintf "Not a bool value: %a" Ast.pp_e f)
          end
          | Lt (m,n) -> let a = interp_e s m in
                        let b = interp_e s n in
                        if a<b then BoolV(true) else BoolV(false)

(* practice & homework *)
let interp (p : Ast.fvae) : Store.value = 
  (* write your code *)
        match p with
        Prog x -> interp_e [] x
