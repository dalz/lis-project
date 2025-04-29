type bop = Sum | Sub | Mul | Div | Mod [@@deriving show]
type uop = Neg [@@deriving show]

type t = Num of int | Var of Ide.t | Bop of bop * t * t | Uop of uop * t
[@@deriving show]

val fv : t -> Ide.t list
val subst : t -> Ide.t -> Ide.t -> t
val pretty : t -> PPrint.document
