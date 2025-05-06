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

type triple = Prop.t * prog * Prop.t
