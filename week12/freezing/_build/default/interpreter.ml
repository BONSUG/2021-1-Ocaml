module F = Format

(* practice & homework *)
let rec interp_e (s : Store.t) (e : Ast.expr) : Store.value = 
  (* write your code *)
        match e with
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
          | Id i ->
                          begin
                          let res = Store.find i s in
                          match res with
                          FreezedV (a,b) -> interp_e b a
                          |ClosureV (d,e,f) -> ClosureV (d,e,f)
                          | NumV g -> NumV (g)
                          end
          | LetIn (j,k,l) ->  interp_e (Store.insert j (interp_e s k) s) l
          | RLetIn (x1,e1,e2) -> 
                          begin
                                  let res1 = interp_e s e1 in
                                  match res1 with
                                  ClosureV(x,y,z) -> 
                                          let rec s1 = (x1,(Store.ClosureV(x,y,s1)))::z in
                                          interp_e s1 e2
                                  | _ -> failwith (Format.asprintf "Not a function: %a" Ast.pp_e e1)
                          end
          | App (q,w) ->
                          begin 
                          let x = interp_e s q in
                          match x with
                          ClosureV (a,b,c) -> 
                                  let fr = Store.FreezedV (w,s) in
                                  let s' = Store.insert a fr c in
                                  interp_e s' b
                          | _-> failwith  (Format.asprintf "Not a function : %a" Ast.pp_e q) 
                          end
          | Fun (x,y) -> ClosureV(x,y,s)
          | Lt (m,n) ->
                          begin
                               let a = interp_e s m in
                               let b = interp_e s n in
                               match a, b with
                               NumV a, NumV b -> 
                                       if a<b then interp_e s (Fun("x",Fun("y",Id("x")))) else  interp_e s (Fun("x",Fun("y",Id("y"))))
                              |_->  failwith (Format.asprintf "Invalid less-than: %a < %a" Ast.pp_e m Ast.pp_e n)
                          end

  
(* practice & homework *)
let interp (p : Ast.fvae) : Store.value = 
  (* write your code *)
        match p with
        Prog x -> interp_e [] x
  

