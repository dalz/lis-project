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

  List.iter
    (fun x ->
      PPrint.ToChannel.pretty 1. 60 Out_channel.stdout
        (Lis_project.Atom.pretty x);
      print_newline ())
    (Lis_project.Simplify.add_equalities atoms_list)

let test_simplify_conj () =
  let () =
    assert_eq "Conj[True, Emp, False, True]"
      (simplify_conj (Conj [ (a_true, Atom Emp, a_false, a_true) ]))
      Conj [ a_false ];
    assert_eq "Conj[True, True]"
      (simplify_conj (Conj [ (a_true, a_true) ]))
      Conj [ a_true ];

    let x_var = Lis_project.Ide.raw_of_string "x" in
    let y_var = Lis_project.Ide.raw_of_string "y" in
    let z_var = Lis_project.Ide.raw_of_string "z" in

    assert_eq "Conj[x->y, True, x->z]"
      (simplify_conj
         (Conj
            [ (Atom (PointsTo (x, Var y)), a_true, Atom (PointsTo (x, Var z))) ]))
      Conj
      [ (Atom (PointsTo (x, Var y)), Atom (Bool (Cmp (Eq, Var y, Var z)))) ]
  in
  ()

let test_simplify_sepj () =
  let () =
    let x_var = Lis_project.Ide.raw_of_string "x" in
    let y_var = Lis_project.Ide.raw_of_string "y" in
    let z_var = Lis_project.Ide.raw_of_string "z" in

    assert_eq "Sepj[Emp, x->y, Emp, x->z]"
      (simplify_sepj
         (Sepj
            [
              ( Conj [ Emp ],
                Conj [ PointsTo (x, Var y) ],
                Conj [ Emp ],
                Conj [ PointsTo (x, Var z) ] );
            ]))
      Sepj [ Conj [ a_false ] ];
    assert_eq "Sepj[Emp, x->y, x->y]"
      (simplify_sepj
         (Sepj
            [
              ( Conj [ Emp ],
                Conj [ PointsTo (x, Var y) ],
                Conj [ PointsTo (x, Var y) ] );
            ]))
      Sepj [ Conj [ a_false ] ]
  in
  ()

let test_simplify_disj () =
  let () =
    let s_true = Sepj [ Conj [ a_true ] ] in
    let s_false = Sepj [ Conj [ a_false ] ] in

    assert_eq "Disj[False, False, False]"
      (simplify_disj (Disj [ (s_false, s_false, s_false) ]))
      Sepj [ s_false ];
    assert_eq "Disj[False, False, True]"
      (simplify_disj (Disj [ (s_false, s_false, s_true) ]))
      Sepj [ s_true ]
  in
  ()

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
