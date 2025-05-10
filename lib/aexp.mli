type bop = Sum | Sub | Mul | Div | Mod [@@deriving show]
type uop = Neg [@@deriving show]

type t = Num of int | Var of Dummy.t | Bop of bop * t * t | Uop of uop * t
[@@deriving show]

val fv : t -> Dummy.t list
val subst : t -> Dummy.t -> Dummy.t -> t
val pretty : t -> PPrint.document
