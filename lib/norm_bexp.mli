open Base

type t = { always : Path_cond.t; alts : Path_cond.t list }
[@@deriving show, equal]

val of_bexp : Bexp.t -> t
