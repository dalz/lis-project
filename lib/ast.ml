open! Base

type ide = string [@@deriving show]
type bop = Sum | Sub | Mul | Div | Mod [@@deriving show]
type uop = Neg [@@deriving show]
type cmpop = Le | Lt | Eq [@@deriving show]

type aexp =
  | Num of int
  | Var of ide
  | Bop of bop * aexp * aexp
  | Uop of uop * aexp
[@@deriving show]

type bexp =
  | Bool of bool
  | Cmp of cmpop * aexp * aexp
  | BAnd of bexp * bexp
  | BOr of bexp * bexp
  | Not of bexp
[@@deriving show]

type atom =
  | True
  | False
  | Bool of bexp
  | PointsTo of ide * aexp
  | Emp
  | PointsToNothing of ide
[@@deriving show]

type pred =
  | Atom of atom
  | And of pred * pred
  | Or of pred * pred
  | Exists of ide * pred
  | Sep of pred * pred
[@@deriving show]

type cmd =
  | Skip
  | Assert of bexp
  | Assign of ide * aexp
  | AssignFromRef of ide * ide
  | AssignToRef of ide * ide
  | Alloc of ide
  | Free of ide
  | Error
[@@deriving show]

type prog =
  | Cmd of cmd
  | Seq of prog * prog
  | Choice of prog * prog
  | Star of prog
[@@deriving show]
