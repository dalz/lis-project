type conj = Conj of Atom.t list [@@deriving show]
type sepj = Sepj of conj list [@@deriving show]
type disj = Disj of sepj list [@@deriving show]
type t = Dummy.t list * disj [@@deriving show]

val of_prop : Prop.t -> prog_vars:Dummy.t list -> t
val simpl : t -> t
val or_ : t -> t -> t
val pretty : t -> PPrint.document
