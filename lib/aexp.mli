type bop = Sum | Sub | Mul | Div | Mod [@@deriving show, equal, compare]
type uop = Neg [@@deriving show, equal, compare]

type t = Num of int | Var of Dummy.t | Bop of bop * t * t | Uop of uop * t
[@@deriving show, equal, compare]

val fv : t -> Dummy.t list
val subst : t -> Dummy.t -> t -> t
val simpl : t -> t
val pretty : t -> PPrint.document
