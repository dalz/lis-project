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

let rec subst (p : t) id id1 =
  match p with
  | Atom (Bool (Const true)) | Atom (Bool (Const false)) | Atom Emp -> p
  | And (p1, p2) -> And (subst p1 id id1, subst p2 id id1)
  | Or (p1, p2) -> Or (subst p1 id id1, subst p2 id id1)
  (* exists: substitute only if id is not shadowed by iden *)
  | Exists (iden, p1) ->
      Exists (iden, if Dummy.equal iden id then p1 else subst p1 id id1)
  | Atom (Bool b1) -> Atom (Bool (Bexp.subst b1 id id1))
  | Atom (PointsTo (iden, a)) ->
      Atom
        (PointsTo
           ((if Dummy.equal iden id then id1 else iden), Aexp.subst a id id1))
  | Atom (PointsToNothing iden) ->
      Atom (PointsToNothing (if Dummy.equal iden id then id1 else iden))
  | Sep (p1, p2) -> Sep (subst p1 id id1, subst p2 id id1)

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
