open Base
open PPrint

type cmp = Le | Lt | Eq [@@deriving show, equal]
type bop = And | Or [@@deriving show, equal]

type t =
  | Const of bool
  | Cmp of cmp * Aexp.t * Aexp.t
  | Bop of bop * t * t
  | Not of t
[@@deriving show, equal]

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
  | Cmp (op, ae1, ae2) -> Cmp (op, Aexp.subst ae1 id id1, Aexp.subst ae2 id id1)
  | Bop (op, b1, b2) -> Bop (op, subst b1 id id1, subst b2 id id1)
  | Not b1 -> Not (subst b1 id id1)

let cmp_to_string = function Le -> "≤" | Lt -> "<" | Eq -> "="

let pretty e =
  let open Pp_util in
  let rec aux t = function
    | Const b -> !^(if b then "⊤" else "⊥")
    | Cmp (op, e1, e2) ->
        pp_bop (Aexp.pretty e1) (cmp_to_string op) (Aexp.pretty e2)
    | Bop (op, e1, e2) ->
        let prec = match op with And -> 1 | Or -> 0 in
        let ops = match op with And -> "&" | Or -> "|" in
        let p = pp_bop (aux prec e1) ops (aux prec e2) in
        if t > prec then parens p else p
    | Not e -> !^"¬" ^^ aux Int.max_value e
  in
  aux (-1) e
