module F = Format

(* Test cases *)
let _ =
  let a = lex['0','1'] (* Int 1 *)
  let b = lex['-','7','9'] (* Int -79 *)
  let c = lex['a','1','T'] (* Var a1T *)
  let d = lex['x','_','\''](* Var x_\' *) (* \' is an escape character for ' *)
  let e = lex['V','a','1']
  let _ = F.printf "a = %a\n" Lexer.pp a in
  let _ = F.printf "b = %a\n" Lexer.pp b in
  let _ = F.printf "c = %a\n" Lexer.pp c in
  let _ = F.printf "d = %a\n" Lexer.pp d in
  let _ = F.printf "e = %a\n" Lexer.pp e in

  try
    let _ = Lexer.lex ['V';'a';'r'] in ()
  with Failure e ->
    F.printf "Exception occurs : %s\n" e (* Exception occurs : Not a valid integer or a valid variable*)

