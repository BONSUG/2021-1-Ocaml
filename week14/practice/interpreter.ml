module F = Format

(* practice & homework *)
let rec interp_e (e : Ast.expr) ((env, mem) : Env.t * Mem.t) : Mem.value = 
        match e with
          | Num n -> NumV n
          | Var v -> Mem.find (Env.find v env) mem
          | Ref  r -> AddressV (Env.find r env)
          | Deref d -> 
                          begin
                                  let adr = Env.find d env in
                                  let a = Mem.find adr mem in 
                                  match a with
                                  AddressV a1 ->Mem.find a1 mem
                                  | _ -> a
                          end
                          
          | Bool b -> BoolV b
          | Add (x,y) -> 
                          begin
                          let a = interp_e x (env,mem) in
                          let b = interp_e y (env,mem) in
                          match a,b with
                          NumV x1,NumV y1 -> NumV (x1+y1)
                         | _ -> failwith (Format.asprintf "Invalid addition: %a + %a" Ast.pp_e x Ast.pp_e y)
                          end
          | Sub (x,y) -> 
                          begin
                          let a = interp_e x (env,mem) in
                          let b = interp_e y (env,mem) in
                          match a,b with
                          NumV x1,NumV y1 -> NumV (x1-y1)
                         | _ -> failwith (Format.asprintf "Invalid subtraction: %a - %a" Ast.pp_e x Ast.pp_e y)
                          end
          | Lt (x,y) -> 
                          begin
                          let a = interp_e x (env,mem) in
                          let b = interp_e y (env,mem) in
                          match a,b with
                          NumV x1,NumV y1 -> if (x1<y1) then BoolV(true) else BoolV(false)
                         | _ -> failwith (Format.asprintf "Invalid less-than: %a < %a" Ast.pp_e x Ast.pp_e y)
                          end

          | Gt (x,y)->   begin
                          let a = interp_e x (env,mem) in
                          let b = interp_e y (env,mem) in
                          match a,b with
                          NumV x1,NumV y1 -> if (x1>y1) then BoolV(true) else BoolV(false)
                         | _ -> failwith (Format.asprintf "Invalid greater-than: %a > %a" Ast.pp_e x Ast.pp_e y)
                          end
          | Eq (x,y) -> 
                          begin
                          let a = interp_e x (env,mem) in
                          let b = interp_e y (env,mem) in
                          match a,b with
                          NumV x1,NumV y1 -> if x1=y1 then BoolV(true) else BoolV(false)
                        |BoolV x1, BoolV y1 -> if x1=y1 then BoolV(true) else BoolV(false)
                        | _ -> failwith (Format.asprintf "Invalid equal- to: %a == %a" Ast.pp_e x Ast.pp_e y)
                          end

          | And (x,y) ->
                          begin
                          let a = interp_e x (env,mem) in
                          let b = interp_e y (env,mem) in
                          match a,b with
                          |BoolV x1, BoolV y1 -> BoolV(x1&&y1)
                        | _ -> failwith (Format.asprintf "Invalid logical-and: %a && %a" Ast.pp_e x Ast.pp_e y)
                          end
          | Or (x,y) ->
                          begin
                          let a = interp_e x (env,mem) in
                          let b = interp_e y (env,mem) in
                          match a,b with
                          |BoolV x1, BoolV y1 -> BoolV(x1||y1)
                        | _ -> failwith (Format.asprintf "Invalid logical-or: %a || %a" Ast.pp_e x Ast.pp_e y)
                          end

(* practice & homework *)
let rec interp_s (stmt : Ast.stmt) ((env, mem) : Env.t * Mem.t) : Env.t * Mem.t = 
        let rec interp1 stmts (env1,mem1) =
                match stmts with
                h::t -> interp1 t (interp_s h (env1,mem1))
                |[]-> (env1,mem1)
        in
         match stmt with
                        | VarDeclStmt x ->
                                        begin
                                        let exist = Env.mem x env in
                                        match exist with
                                        |true -> failwith (Format.asprintf "%s is already declared." x) 
                                        |false -> 
                                                        begin
                                                        let a = (Env.new_address ()) in
                                                        let e1 = Env.insert x a env in
                                                        (e1,mem)
                                                        end
                                        end

                        | StoreStmt (e1,e2) ->
                                                begin
                                                        let a = interp_e e1 (env,mem) in
                                                        let v = interp_e e2 (env,mem) in
                                                        match a with
                                                        AddressV a1 -> (env,(Mem.insert a1 v mem))
                                                        | _ -> failwith (Format.asprintf "Not a memory address : %a" Ast.pp_e e1)
                                                end
                        | WhileStmt (e,slist) ->
                                        begin
                                                let res = interp_e e (env,mem) in
                                                match res with
                                                BoolV true ->   begin                                      
                                                                match slist with
                                                                h::t -> 
                                                                        let (a,b) = interp1 (h::t) (env,mem) in
                                                                        interp_s (WhileStmt(e,slist)) (a,b)
                                                                        
                                                                |[] -> (env,mem)
                                                end
                                                |BoolV false -> (env,mem)
                                                | _ ->failwith (Format.asprintf "Not a boolean : %a" Ast.pp_e e)                                                      
                                        end
                        |IfStmt (e,sl ,op) ->
                                       begin
                                       let res = interp_e e (env,mem) in
                                       match res with
                                       BoolV true ->
                                                  begin
                                                  match sl with
                                                  h::t -> interp1 (h::t) (env,mem)
                                                  |[] -> (env,mem)
                                                  end
                                        |BoolV false -> 
                                                        begin
                                                        match op with
                                                        Some op ->
                                                                begin
                                                                  match op with
                                                                  h::t -> interp1 (h::t)  (env,mem)
                                                                 | [] -> (env,mem)
                                                                 end 
                                                         |None -> (env,mem)
                                                         end
                                          |_-> failwith (Format.asprintf "Not a boolean : %a" Ast.pp_e e) 

                                         end


(* practice & homework *)
let interp (p : Ast.program) : Env.t * Mem.t =
        let rec interp1 stmts (env,mem) =
               match stmts with
               h::t -> interp1 t (interp_s h (env,mem))
               |[]-> (env,mem)
        in
        match p with
        Program x ->interp1 x ([],[])
 
 
