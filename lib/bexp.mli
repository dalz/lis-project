type cmp = Le | Lt | Eq
type bop = And | Or

type t =
  | Const of bool
  | Cmp of cmp * Aexp.t * Aexp.t
  | Bop of bop * t * t
  | Not of t
[@@deriving show]

val fv : t -> Dummy.t list
val subst : t -> Dummy.t -> Dummy.t -> t
val cmp_to_string : cmp -> string
val pretty : t -> PPrint.document
