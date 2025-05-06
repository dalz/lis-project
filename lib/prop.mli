type t =
  | Atom of Atom.t
  | And of t * t
  | Or of t * t
  | Exists of Ide.t * t
  | Sep of t * t
[@@deriving show]

val subst : t -> Ide.t -> Ide.t -> t
val pretty : t -> PPrint.document
