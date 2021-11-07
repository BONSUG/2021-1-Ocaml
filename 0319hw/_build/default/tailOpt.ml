module F = Format

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

