type conj = Conj of Atom.t list
type sepj = Sepj of conj list
type disj = Disj of sepj list
type t = Ide.t list * disj

val of_prop : Ast.prop -> t
