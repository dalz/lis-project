type atom = Const of bool | Cmp of Cmp.t * Aexp.t * Aexp.t
[@@deriving show, equal]

type t = atom list [@@deriving show, equal]

val subst : ?noloop:bool -> t -> Dummy.t -> Aexp.t -> t
val is_false : t -> bool
val simpl : t -> t
val get_substs : t -> (Dummy.t * Aexp.t) list
val is_null : t -> Dummy.t -> bool
val pretty : t -> PPrint.document
