open Base

type t [@@deriving show, equal, compare, sexp_of, hash]

val to_string : t -> string
val raw_of_string : string -> t
val assert_raw : t -> unit
val fresh_of_string : string -> t
val fresh_of_t : t -> t
val set_done_parsing : unit -> unit

type comparator_witness

val comparator : (t, comparator_witness) Comparator.t
