type t = Cmd of Cmd.t | Seq of t * t | Choice of t * t | Iter of t
[@@deriving show]

val fv : t -> Ide.t list * Dummy.t list
val pretty : t -> PPrint.document
