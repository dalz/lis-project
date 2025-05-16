type t =
  | Skip
  | Assert of Bexp.t
  | Assign of Ide.t * Aexp.t
  | AssignFromRef of Ide.t * Ide.t
  | AssignToRef of Ide.t * Ide.t
  | Alloc of Ide.t
  | Free of Ide.t
  | Error
[@@deriving show]

val fv : t -> Ide.t list * Dummy.t list
val pretty : t -> PPrint.document
