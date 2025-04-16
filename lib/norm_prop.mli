type conj = Conj of Ast.atom list
type sepj = Sepj of conj list
type disj = Disj of sepj list
type t = Ide.t list * disj

val of_prop : Ast.prop -> t
val show : t -> string
