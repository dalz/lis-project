open Base

type t [@@deriving show, equal]

val to_string : t -> string
val raw : string -> int -> t
val raw_of_ide : ?force:bool -> Ide.t -> t
val fresh_of_ide : Ide.t -> t
val fresh_of_t : t -> t
val is_ide : t -> bool
val get_ide : t -> Ide.t

type comparator_witness

val comparator : (t, comparator_witness) Comparator.t
