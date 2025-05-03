open Lis_project.Ast
open Lis_project.Simplify

let a_true = Atom (Bool (BConst true))
let a_false = Atom (Bool (BConst false))

(* let a_bool_true = Atom (Bool (Bool true))
let a_bool_false = Atom (Bool (Bool false)) *)

let assert_eq (name : string) (got : prop) (expected : prop) =
  if got = expected then Printf.printf "PASS: %s\n" name
  else
    Printf.printf "FAIL: %s\n  got: %s\n  expected: %s\n" name (show_prop got)
      (show_prop expected)

let test_remove_multiple_pointsto = 
  let atoms_list = [
                  Lis_project.Atom.PointsTo (Lis_project.Ide.raw_of_string "x", Var(Lis_project.Ide.raw_of_string "y"));
                  Lis_project.Atom.PointsTo (Lis_project.Ide.raw_of_string "x", Num(5));
                  Lis_project.Atom.PointsTo (Lis_project.Ide.raw_of_string "w", Var(Lis_project.Ide.raw_of_string "y"));
                  Lis_project.Atom.PointsTo (Lis_project.Ide.raw_of_string "w", Var(Lis_project.Ide.raw_of_string "z"));
                  Lis_project.Atom.PointsTo (Lis_project.Ide.raw_of_string "x", Lis_project.Aexp.Bop(Sum, Num(2), Num(3)));
                  ] in

List.iter (fun x -> (PPrint.ToChannel.pretty 1. 60 Out_channel.stdout(Lis_project.Atom.pretty x)); print_newline ()) (Lis_project.Simplify.add_equalities atoms_list) ;;

let () =
  assert_eq "And(True, True)" (simplify_prop (And (a_true, a_true))) a_true;

  assert_eq "And(True, False)" (simplify_prop (And (a_true, a_false))) a_false;

  assert_eq "And(False, False)" (simplify_prop (And (a_false, a_false))) a_false;

  assert_eq "And(False, True)" (simplify_prop (And (a_false, a_true))) a_false;

  assert_eq "And(False, False)" (simplify_prop (And (a_false, a_false))) a_false;

  assert_eq "Or(True, True)" (simplify_prop (Or (a_true, a_true))) a_true;

  assert_eq "Or(True, False)" (simplify_prop (Or (a_true, a_false))) a_true;

  assert_eq "Or(False, True)" (simplify_prop (Or (a_false, a_true))) a_true;

  assert_eq "Or(False, False)" (simplify_prop (Or (a_false, a_false))) a_false;

  assert_eq "Nested And Or"
    (simplify_prop (And (a_true, Or (a_false, a_true))))
    a_true;

  assert_eq "Exists(x, True)"
    (simplify_prop (Exists (Lis_project.Ide.raw_of_string "x", a_true)))
    a_true;

  assert_eq "Exists(x, False)"
    (simplify_prop (Exists (Lis_project.Ide.raw_of_string "x", a_false)))
    a_false;

  assert_eq "Cmp(Le, 1, 2)"
    (simplify_prop (Atom (Bool (Cmp (Le, Num 1, Num 2)))))
    a_true;

  assert_eq "Cmp(Le, 2, 1)"
    (simplify_prop (Atom (Bool (Cmp (Le, Num 2, Num 1)))))
    a_false;

  assert_eq "Sep(True, empty)" (simplify_prop (Sep (a_true, Atom Emp))) a_true;

  assert_eq "Sep(False, empty)"
    (simplify_prop (Sep (a_false, Atom Emp)))
    a_false;

  assert_eq "Sep(empty, True)" (simplify_prop (Sep (Atom Emp, a_true))) a_true;

  assert_eq "Sep(empty, False)"
    (simplify_prop (Sep (Atom Emp, a_false)))
    a_false
