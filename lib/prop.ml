open Base
open PPrint
open Pp_util

type t =
  | Atom of Atom.t
  | And of t * t
  | Or of t * t
  | Exists of Dummy.t * t
  | Sep of t * t
[@@deriving show]

let rec fv = function
  | Atom a -> Atom.fv a
  | And (p1, p2) | Or (p1, p2) | Sep (p1, p2) -> fv p1 @ fv p2
  | Exists (x, p) -> fv p |> List.filter ~f:(Fn.non (Dummy.equal x))

let rec subst (p : t) id id1 =
  match p with
  | And (p1, p2) -> And (subst p1 id id1, subst p2 id id1)
  | Or (p1, p2) -> Or (subst p1 id id1, subst p2 id id1)
  (* exists: substitute only if id is not shadowed by iden *)
  | Exists (iden, p1) ->
      Exists (iden, if Dummy.equal iden id then p1 else subst p1 id id1)
  | Sep (p1, p2) -> Sep (subst p1 id id1, subst p2 id id1)
  | Atom a -> Atom (Atom.subst a id id1)

(* very basic, just to remove some noise from the result of the SL executor *)
let rec simpl = function
  | Exists _ as p -> p
  | Atom a -> Atom (Atom.simpl a)
  | And (p, q) -> (
      match (simpl p, simpl q) with
      | p, Atom (Emp | Bool (Const true)) | Atom (Emp | Bool (Const true)), p ->
          p
      | _, Atom (Bool (Const false)) | Atom (Bool (Const false)), _ ->
          Atom (Bool (Const false))
      | p, q -> And (p, q))
  | Sep (p, q) -> (
      match (simpl p, simpl q) with
      | p, Atom (Emp | Bool (Const true)) | Atom (Emp | Bool (Const true)), p ->
          p
      | _, Atom (Bool (Const false)) | Atom (Bool (Const false)), _ ->
          Atom (Bool (Const false))
      | p, q -> Sep (p, q))
  | Or (p, q) -> (
      match (simpl p, simpl q) with
      | _, Atom (Emp | Bool (Const true)) | Atom (Emp | Bool (Const true)), _ ->
          Atom (Bool (Const false))
      | p, Atom (Bool (Const false)) | Atom (Bool (Const false)), p -> p
      | p, q -> Or (p, q))

let pretty p =
  let rec aux t = function
    | Atom a -> Atom.pretty a
    | And (p1, p2) -> pp_assoc_bop aux 3 t p1 "∧" p2
    | Or (p1, p2) -> pp_assoc_bop aux 1 t p1 "∨" p2
    | Sep (p1, p2) -> pp_assoc_bop aux 2 t p1 "∗" p2
    | Exists (x, p) ->
        let prec = 0 in
        let x =
          group
            (utf8string "∃" ^^ space
            ^^ align (utf8string (Dummy.to_string x ^ ",") ^/^ aux (prec - 1) p)
            )
        in
        if t >= prec then parens x else x
  in
  aux (-1) p
