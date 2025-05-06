type conj = Conj of Atom.t list [@@deriving show]
type sepj = Sepj of conj list [@@deriving show]
type disj = Disj of sepj list [@@deriving show]
type t = Ide.t list * disj [@@deriving show]

val of_prop : Prop.t -> t
val pretty : t -> PPrint.document
