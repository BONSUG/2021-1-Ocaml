module F=Format

let split li= 
        let rec split2 li acc1 acc2 =
                match li with
                [] -> (List.rev acc1,List.rev acc2)
                | (x,y)::t -> split2 t (x::acc1) (y::acc2)
        in
        split2 li [] []

let combine li1 li2 =
        let rec combine2 li1 li2 acc =
                match li1, li2 with
                [], [] -> List.rev acc
                | (x::t1), (y::t2) -> combine2 (t1) (t2) ((x,y)::acc)
                | _ -> failwith "err"
        in
        combine2 li1 li2 []


let prt (x,y) =
             let _ = F.printf "([" in
             let _ =List.iter (fun i -> F.printf " %d " i ) x in
             let _ = F.printf "] [" in
             let _ = List.iter (fun j -> F.printf " %d " j ) y in
             F.printf "])\n"

let print_combine l =
        let _ = F.printf "[" in
        let _ = List.iter (fun (x,y) ->F.printf " (%d,%d) " x y ) l in       
        F.printf "]\n"
        
let _ =
        let res = split [(1,2);(3,4);(5,6)] in
        let _ = prt res in
        let result = combine [1;2;3] [4;5;6] in
        print_combine result

