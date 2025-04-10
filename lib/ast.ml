open! Base

type ide = string
type bop = Sum | Sub | Mul | Div | Mod
type uop = Neg
type cmpop = Le | Lt | Eq

type aexp =
  | Num of int
  | Var of ide
  | Bop of bop * aexp * aexp
  | Uop of uop * aexp

type bexp =
  | Bool of bool
  | Cmp of cmpop * aexp * aexp
  | BAnd of bexp * bexp
  | BOr of bexp * bexp
  | Not of bexp

type pred =
  | True
  | False
  | And of pred * pred
  | Or of pred * pred
  | Exists of ide * pred
  | Bool of bexp
  | Emp
  | PointsTo of ide * aexp
  | PointsToNothing of ide
  | Sep of pred * pred

type cmd =
  | Skip
  | Assert of bexp
  | Assign of ide * aexp
  | AssignFromRef of ide * ide
  | AssignToRef of ide * ide
  | Alloc of ide
  | Free of ide
  | Error

type prog =
  | Cmd of cmd
  | Seq of prog * prog
  | Choice of prog * prog
  | Star of prog
