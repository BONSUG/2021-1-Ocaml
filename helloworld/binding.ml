let y = 2 in
let f x = x + y in
let f x = let y = 3 in f y in
let res = f 5 in
Format.printf "%d\n" res
