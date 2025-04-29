type t =
  | Bool of Bexp.t
  | PointsTo of Ide.t * Aexp.t
  | Emp
  | PointsToNothing of Ide.t
[@@deriving show]

val fv : t -> Ide.t list
val pretty : t -> PPrint.document
