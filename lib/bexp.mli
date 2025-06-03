type bop = And | Or [@@deriving show, equal, compare]

type t =
  | Const of bool
  | Cmp of Cmp.t * Aexp.t * Aexp.t
  | Bop of bop * t * t
  | Not of t
[@@deriving show, equal, compare]

val fv : t -> Dummy.t list
val subst : t -> Dummy.t -> Dummy.t -> t
val simpl : t -> t
val pretty : t -> PPrint.document
