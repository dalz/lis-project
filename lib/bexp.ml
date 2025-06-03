open Base
open PPrint

type bop = And | Or [@@deriving show, equal, compare]

type t =
  | Const of bool
  | Cmp of Cmp.t * Aexp.t * Aexp.t
  | Bop of bop * t * t
  | Not of t
[@@deriving show, equal, compare]

let fv e =
  let rec aux e xs =
    match e with
    | Cmp (_, e1, e2) -> Aexp.fv e1 @ Aexp.fv e2
    | Bop (_, e1, e2) -> aux e1 xs |> aux e2
    | Not e -> aux e xs
    | Const _ -> xs
  in
  aux e []

let rec subst (b : t) id id1 =
  match b with
  | Const _ -> b
  | Cmp (op, ae1, ae2) ->
      Cmp (op, Aexp.subst ae1 id (Var id1), Aexp.subst ae2 id (Var id1))
  | Bop (op, b1, b2) -> Bop (op, subst b1 id id1, subst b2 id id1)
  | Not b1 -> Not (subst b1 id id1)

(* very basic, we have a better simplification procedure for norm_bexp *)
let rec simpl = function
  | Const _ as b -> b
  | Cmp (op, a, b) -> (
      match (Aexp.simpl a, Aexp.simpl b) with
      | Num n, Num m -> Const (Cmp.compute op n m)
      | a, b -> Cmp (op, a, b))
  | Bop (And, p, q) -> (
      match (simpl p, simpl q) with
      | p, Const true | Const true, p -> p
      | _, Const false | Const false, _ -> Const false
      | p, q -> Bop (And, p, q))
  | Bop (Or, p, q) -> (
      match (simpl p, simpl q) with
      | p, Const false | Const false, p -> p
      | _, Const true | Const true, _ -> Const true
      | p, q -> Bop (Or, p, q))
  | Not p -> ( match simpl p with Const b -> Const (not b) | p -> Not p)

let pretty e =
  let open Pp_util in
  let rec aux t = function
    | Const b -> !^(if b then "⊤" else "⊥")
    | Cmp (op, e1, e2) ->
        pp_bop (Aexp.pretty e1) (Cmp.to_string op) (Aexp.pretty e2)
    | Bop (op, e1, e2) ->
        let prec = match op with And -> 1 | Or -> 0 in
        let ops = match op with And -> "&" | Or -> "|" in
        let p = pp_bop (aux prec e1) ops (aux prec e2) in
        if t > prec then parens p else p
    | Not e -> !^"¬(" ^^ aux Int.max_value e ^^ !^")"
  in
  aux (-1) e
