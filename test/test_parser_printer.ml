open Base
open Stdio
open Lis_project

let f = 1.25
let x = Dummy.raw "x" 0
let pick opts = List.length opts |> Random.int |> List.nth_exn opts
let decide p = Float.(Random.float 1. < p)

let rec gen_aexp lp =
  let open Aexp in
  let lp' = lp *. f in
  if decide lp then pick [ Num 1; Var x ]
  else if decide 0.8 then
    Bop (pick [ Sum; Sub; Mul; Div; Mod ], gen_aexp lp', gen_aexp lp')
  else Uop (Neg, gen_aexp lp')

let rec gen_bexp lp =
  let open Bexp in
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
  let open Atom in
  let lp' = lp *. f in
  if decide lp then pick [ Emp; PointsToNothing x ]
  else if decide 0.5 then Bool (gen_bexp lp')
  else PointsTo (x, gen_aexp lp')

let rec gen_prop lp =
  let open Prop in
  let lp' = lp *. f in
  if decide lp then Atom (gen_atom lp')
  else
    pick
      [
        (fun () -> And (gen_prop lp', gen_prop lp'));
        (fun () -> Or (gen_prop lp', gen_prop lp'));
        (fun () -> Sep (gen_prop lp', gen_prop lp'));
        (fun () -> Exists (x, gen_prop lp'));
      ]
      ()

let gen_norm_prop lp = Norm_prop.of_prop (gen_prop lp)

let test_parser_printer () =
  Random.self_init ();
  for _ = 0 to 5 do
    let e = gen_norm_prop 0.09 in
    Out_channel.print_endline (Norm_prop.show e);
    PPrint.ToChannel.pretty 1. 60 Out_channel.stdout (Norm_prop.pretty e);
    Out_channel.print_endline "\n"
  done
