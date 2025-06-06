type t =
  | Atom of Atom.t
  | And of t * t
  | Or of t * t
  | Exists of Dummy.t * t
  | Sep of t * t
[@@deriving show]

val fv : t -> Dummy.t list
val subst : t -> Dummy.t -> Dummy.t -> t
val simpl : t -> t
val pretty : t -> PPrint.document
