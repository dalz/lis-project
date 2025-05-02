open Lis_project
open Ast

(* let () =
  let t = Atom True in
  let x = Ide.raw_of_string "x" in
  let y = Ide.raw_of_string "x" in
  let ptn x = Atom (PointsToNothing x) in
  let p =
    And
      ( Sep (Exists (x, Or (t, ptn x)), t),
        Or
          ( And (Exists (x, ptn x), Sep (Exists (y, Exists (y, ptn y)), ptn y)),
            t ) )
  in
  Norm_prop.of_prop p |> Norm_prop.show |> print_endline *)

let parse_proposition (s : string) : prop =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.eprop Lexer.read lexbuf in
  ast

let parse_program (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.eprog Lexer.read lexbuf in
  ast
