type prop =
  | Atom of Atom.t
  | And of prop * prop
  | Or of prop * prop
  | Exists of Ide.t * prop
  | Sep of prop * prop

type cmd =
  | Skip
  | Assert of Bexp.t
  | Assign of Ide.t * Aexp.t
  | AssignFromRef of Ide.t * Ide.t
  | AssignToRef of Ide.t * Ide.t
  | Alloc of Ide.t
  | Free of Ide.t
  | Error

type prog =
  | Cmd of cmd
  | Seq of prog * prog
  | Choice of prog * prog
  | Star of prog

type triple = prop * prog * prop
