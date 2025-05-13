type t =
  | Atom of Atom.t
  | And of t * t
  | Or of t * t
  | Exists of Dummy.t * t
  | Sep of t * t
[@@deriving show]

val subst : t -> Dummy.t -> Dummy.t -> t
val pretty : t -> PPrint.document
