module F = Format

(* practice & homework *)
let rec interp_e (e : Ast.expr) (s : Store.t) : Store.value = 
        match e with
          | Num n -> NumV n
          | Var v -> Store.find v s
          | Bool b -> BoolV b
          | Add (x,y) -> 
                          begin
                          let a = interp_e x s in
                          let b = interp_e y s in
                          match a,b with
                          NumV x1,NumV y1 -> NumV (x1+y1)
                         | _ -> failwith (Format.asprintf "Invalid addition: %a + %a" Ast.pp_e x Ast.pp_e y)
                          end
          | Sub (x,y) -> 
                          begin
                          let a = interp_e x s in
                          let b = interp_e y s in
                          match a,b with
                          NumV x1,NumV y1 -> NumV (x1-y1)
                         | _ -> failwith (Format.asprintf "Invalid subtraction: %a - %a" Ast.pp_e x Ast.pp_e y)
                          end
          | Lt (x,y) -> 
                          begin
                          let a = interp_e x s in
                          let b = interp_e y s in
                          match a,b with
                          NumV x1,NumV y1 -> if (x1<y1) then BoolV(true) else BoolV(false)
                         | _ -> failwith (Format.asprintf "Invalid less-than: %a < %a" Ast.pp_e x Ast.pp_e y)
                          end

          | Gt (x,y)->   begin
                          let a = interp_e x s in
                          let b = interp_e y s in
                          match a,b with
                          NumV x1,NumV y1 -> if (x1>y1) then BoolV(true) else BoolV(false)
                         | _ -> failwith (Format.asprintf "Invalid greater-than: %a > %a" Ast.pp_e x Ast.pp_e y)
                          end
          | Eq (x,y) -> 
                          begin
                          let a = interp_e x s in
                          let b = interp_e y s in
                          match a,b with
                          NumV x1,NumV y1 -> if x1=y1 then BoolV(true) else BoolV(false)
                        |BoolV x1, BoolV y1 -> if x1=y1 then BoolV(true) else BoolV(false)
                        | _ -> failwith (Format.asprintf "Invalid equal- to: %a == %a" Ast.pp_e x Ast.pp_e y)
                          end

          | And (x,y) ->
                          begin
                          let a = interp_e x s in
                          let b = interp_e y s in
                          match a,b with
                          |BoolV x1, BoolV y1 -> BoolV(x1&&y1)
                        | _ -> failwith (Format.asprintf "Invalid logical-and: %a && %a" Ast.pp_e x Ast.pp_e y)
                          end
          | Or (x,y) ->
                          begin
                          let a = interp_e x s in
                          let b = interp_e y s in
                          match a,b with
                          |BoolV x1, BoolV y1 -> BoolV(x1||y1)
                        | _ -> failwith (Format.asprintf "Invalid logical-or: %a || %a" Ast.pp_e x Ast.pp_e y)
                          end

(* practice & homework *)
let rec interp_s (stmt : Ast.stmt) (s : Store.t) : Store.t = 
          match stmt with
                        | AssignStmt (x,e) -> 
                          begin
                          let res = interp_e e s in
                          Store.insert x res s 
                          end
                                                          
                       | IfStmt (e,sl ,op) ->
                                       begin
                                            let rec interp1 stmts s1 =
                                            match stmts with
                                               h::t -> interp1 t (interp_s h s1)
                                               |[]-> s1
                                       in
                                       let res = interp_e e s in
                                       match res with
                                       BoolV true ->
                                                  begin
                                                  match sl with
                                                  h::t -> interp1 (h::t) s
                                                  |[] -> s
                                                  end
                                        |BoolV false -> 
                                                          begin
                                                        match op with
                                                        Some op ->
                                                                begin
                                                                  match op with
                                                                  h::t -> interp1 (h::t) s
                                                                 | [] -> s
                                                                 end 
                                                                |None -> s
                                                           end
                                          |_-> failwith (Format.asprintf "Not a boolean : %a" Ast.pp_e e) 

                                         end


(* practice & homework *)
let interp (p : Ast.program) : Store.t = 
        let rec interp1 stmts s1 =
               match stmts with
               h::t -> interp1 t (interp_s h s1)
               |[]-> s1
        in
        match p with
        Program x ->interp1 x []
