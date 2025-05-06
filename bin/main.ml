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

(* let test_prop = parse_proposition "" in
Simplify. *)

let test_remove_multiple_pointsto =
  let atoms_list =
    [
      Lis_project.Atom.PointsTo
        ( Lis_project.Ide.raw_of_string "x",
          Var (Lis_project.Ide.raw_of_string "y") );
      Lis_project.Atom.PointsTo (Lis_project.Ide.raw_of_string "x", Num 5);
      Lis_project.Atom.PointsTo
        ( Lis_project.Ide.raw_of_string "w",
          Var (Lis_project.Ide.raw_of_string "y") );
      Lis_project.Atom.PointsTo
        ( Lis_project.Ide.raw_of_string "w",
          Var (Lis_project.Ide.raw_of_string "z") );
      Lis_project.Atom.PointsTo
        ( Lis_project.Ide.raw_of_string "x",
          Lis_project.Aexp.Bop (Sum, Num 2, Num 3) );
    ]
  in

  let simplified =
    Lis_project.Simplify.simplify_conj (Lis_project.Norm_prop.Conj atoms_list)
  in

  let atoms_list =
    match simplified with Lis_project.Norm_prop.Conj atoms_list -> atoms_list
  in
  let _ =
    List.iter
      (fun x ->
        PPrint.ToChannel.pretty 1. 60 Out_channel.stdout
          (Lis_project.Atom.pretty x);
        print_newline ())
      atoms_list
  in

  let atom_test_list =
    [
      Lis_project.Atom.PointsTo
        ( Lis_project.Ide.raw_of_string "x",
          Lis_project.Aexp.Bop (Sum, Num 2, Num 3) );
      Lis_project.Atom.PointsTo
        ( Lis_project.Ide.raw_of_string "x",
          Lis_project.Aexp.Bop (Div, Num 4, Num 2) );
      Lis_project.Atom.PointsTo
        ( Lis_project.Ide.raw_of_string "x",
          Lis_project.Aexp.Bop (Mul, Num 3, Num 2) );
      Lis_project.Atom.PointsTo
        ( Lis_project.Ide.raw_of_string "x",
          Lis_project.Aexp.Bop (Mod, Num 6, Num 3) );
      Lis_project.Atom.PointsTo
        ( Lis_project.Ide.raw_of_string "x",
          Lis_project.Aexp.Uop (Neg, Lis_project.Aexp.Bop (Mod, Num 6, Num 3))
        );
      Lis_project.Atom.PointsTo
        ( Lis_project.Ide.raw_of_string "x",
          Lis_project.Aexp.Uop (Neg, Lis_project.Aexp.Uop (Neg, Num 6)) );
      Lis_project.Atom.PointsTo
        ( Lis_project.Ide.raw_of_string "x",
          Lis_project.Aexp.Var (Lis_project.Ide.raw_of_string "x") );
    ]
  in

  ()

let () = test_remove_multiple_pointsto
