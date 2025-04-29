open Base
open Stdio
open Lis_project
open Aexp
open Bexp
open Atom

let f = 1.3
let x = Ide.raw_of_string "x"
let pick opts = List.length opts |> Random.int |> List.nth_exn opts
let decide p = Float.(Random.float 1. < p)

let rec gen_aexp lp =
  let lp' = lp *. f in
  if decide lp then pick [ Num 1; Var x ]
  else if decide 0.8 then
    Bop (pick [ Sum; Sub; Mul; Div; Mod ], gen_aexp lp', gen_aexp lp')
  else Uop (Neg, gen_aexp lp')

let rec gen_bexp lp =
  let lp' = lp *. f in
  if decide lp then Const (decide 0.5)
  else
    pick
      [
        (fun () -> Cmp (pick [ Le; Lt; Eq ], gen_aexp lp', gen_aexp lp'));
        (fun () -> Bop (pick [ And; Or ], gen_bexp lp', gen_bexp lp'));
        (fun () -> Not (gen_bexp lp'));
      ]
      ()

let gen_atom lp =
  let lp' = lp *. f in
  if decide lp then pick [ Emp; PointsToNothing x ]
  else if decide 0.5 then Bool (gen_bexp lp')
  else PointsTo (x, gen_aexp lp')

let test_parser_printer () =
  Random.self_init ();
  for _ = 0 to 5 do
    let e = gen_atom 0.1 in
    Out_channel.print_endline (show e);
    PPrint.ToChannel.pretty 1. 60 Out_channel.stdout (pretty e);
    Out_channel.print_endline "\n"
  done
