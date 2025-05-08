type t = Cmd of Cmd.t | Seq of t * t | Choice of t * t | Star of t
[@@deriving show]

val pretty : t -> PPrint.document
