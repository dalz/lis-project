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

let _parse_proposition (s : string) : prop =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.eprop Lexer.read lexbuf in
  ast

let _parse_program (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.eprog Lexer.read lexbuf in
  ast

let () =
  let propositions_to_test =
    [ "exists x. emp" ]
    (*
  Both true and false return an empty list, is this ok???
  "exists x. true" -> [] (works)
  "exists x. false" -> [] (works)
  "exists x. true || false" -> [] (works)
  "exists x. true && false" -> NOT SURE, STRANGE OUTPUT, DOUBLE CHECK
  "exists x. true * true" -> [] (works)
  "exists x. true * false" -> [false] conj [] shouldn't the second empty list be removed? 
  "exists x. y -> 3 + 2" -> fails, correct?? 
  "exists x. (true * x -> 5)" -> fails, correct??
  "exists x. emp" -> fails, correct??
  "exists x. true * emp" -> [] works
  "exists x. emp * true" -> fails, correct??
  "x-> 5 * y -> 3" -> no changes (works)
  "x-> 5 * y -> 3 + 2" -> x -> 5 * y-> 5 (works)
  "false * true" -> false (works)
  "false * true" -> false (works but leaves the empty list behind like the case above, shouldn't that be removed?)
  "x -> 5 * y -> 10" -> same (works)
  "x -> 5 * x -> 5" -> x -> 5 (works) 
  "x -> 5 * x -> 5 * x -> 5" -> x -> 5 (works)
  "emp * x -> 5" -> x -> 5 (works)
  "x -> 5 * emp" -> x -> 5 (works)
  "emp * emp * emp" -> emp (works)
  "x -> 5 * emp * emp * emp" -> x -> 5 (works)
  *)
  in
  List.iter
    (fun x ->
      let parsed_prop =
        parse_proposition x |> Lis_project.Simplify.simplify_prop
      in
      let normalized_ast = Lis_project.Norm_prop.of_prop parsed_prop in
      let simplified = Lis_project.Simplify.simplify_t normalized_ast in
      print_string "\nProp after second simplification\n";
      print_string (Lis_project.Norm_prop.show simplified);
      print_newline ())
    propositions_to_test
