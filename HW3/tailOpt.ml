module F = Format

let combine li1 li2 =
                let rec combine2 li1 li2 acc =
                        match li1, li2 with
                        [], [] -> List.rev acc
                        | (x::t1), (y::t2) -> combine2 (t1) (t2) ((x,y)::acc)
                        | _ -> failwith "err"
                in
                combine2 li1 li2 []

F.printf "%a" combine [1;2] [3;4] 
