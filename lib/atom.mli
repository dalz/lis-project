type t =
  | Bool of Bexp.t
  | Emp
  | PointsTo of Dummy.t * Aexp.t
  | PointsToNothing of Dummy.t
  | PointsToUndefined of Dummy.t
[@@deriving show]

val fv : t -> Dummy.t list
val subst : t -> Dummy.t -> Dummy.t -> t
val pretty : t -> PPrint.document
