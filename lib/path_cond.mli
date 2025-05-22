type atom = Const of bool | Cmp of Cmp.t * Aexp.t * Aexp.t
[@@deriving show, equal]

type t = atom list [@@deriving show, equal]

val subst : t -> Dummy.t -> Dummy.t -> t
val is_false : t -> bool
val simpl : t -> t
val pretty : t -> PPrint.document
