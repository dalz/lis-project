type bop = Sum | Sub | Mul | Div | Mod [@@deriving show]
type uop = Neg [@@deriving show]
type cmpop = Le | Lt | Eq [@@deriving show]

type aexp =
  | Num of int
  | Var of Ide.t
  | Bop of bop * aexp * aexp
  | Uop of uop * aexp
[@@deriving show]

type bexp =
  | BConst of bool
  | Cmp of cmpop * aexp * aexp
  | BAnd of bexp * bexp
  | BOr of bexp * bexp
  | Not of bexp
[@@deriving show]

type atom =
  | Bool of bexp
  | PointsTo of Ide.t * aexp
  | Emp
  | PointsToNothing of Ide.t
[@@deriving show]

type prop =
  | Atom of atom
  | And of prop * prop
  | Or of prop * prop
  | Exists of Ide.t * prop
  | Sep of prop * prop
[@@deriving show]

type cmd =
  | Skip
  | Assert of bexp
  | Assign of Ide.t * aexp
  | AssignFromRef of Ide.t * Ide.t
  | AssignToRef of Ide.t * Ide.t
  | Alloc of Ide.t
  | Free of Ide.t
  | Error
[@@deriving show]

type prog =
  | Cmd of cmd
  | Seq of prog * prog
  | Choice of prog * prog
  | Star of prog
[@@deriving show]
