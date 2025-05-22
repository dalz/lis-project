open Base
open PPrint

type atom = Const of bool | Cmp of Cmp.t * Aexp.t * Aexp.t
[@@deriving show, equal]

type t = atom list [@@deriving show, equal]

let subst_atom a x x' =
  match a with
  | Const _ -> a
  | Cmp (op, e1, e2) -> Cmp (op, Aexp.subst e1 x x', Aexp.subst e2 x x')

let subst p x x' = List.map p ~f:(fun a -> subst_atom a x x')
let is_false = function [ Const false ] -> true | _ -> false

(*

  x + 1 = y + 2
  x = y + 1

  1 = x
  0 = x - 1

  

*)

let simpl_cmp op a b =
  let a = Aexp.simpl (Bop (Sub, a, b)) in
  Cmp (op, a, Num 0)

let simpl p =
  let p =
    List.fold_until p ~init:[] ~finish:Fn.id ~f:(fun acc -> function
      | Const true -> Continue acc
      | Const false as a -> Stop [ a ]
      | Cmp (op, a, b) -> Continue (simpl_cmp op a b :: acc))
    |> List.rev
  in
  p

let pretty p =
  separate_map
    (break 1 ^^ utf8string "∧" ^^ space)
    (function
      | Const b -> Bexp.pretty (Const b)
      | Cmp (op, e1, e2) -> Bexp.pretty (Cmp (op, e1, e2)))
    p
