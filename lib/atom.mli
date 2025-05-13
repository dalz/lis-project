type t =
  | Bool of Bexp.t
  | PointsTo of Dummy.t * Aexp.t
  | Emp
  | PointsToNothing of Dummy.t
[@@deriving show]

val fv : t -> Dummy.t list
val pretty : t -> PPrint.document
