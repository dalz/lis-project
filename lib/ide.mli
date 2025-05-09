open Base

type t [@@deriving show, compare, sexp_of, hash]

val to_string : t -> string
val raw_of_string : string -> t
val fresh_of_string : string -> t
val fresh_of_t : t -> t
val equal : t -> t -> bool
val ( = ) : t -> t -> bool
