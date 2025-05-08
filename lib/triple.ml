open PPrint

type t = Prop.t * Prog.t * Prop.t

let pretty (pre, prog, post) =
  group
    (align
       (!^"{"
       ^^ align (Prop.pretty pre ^^ !^"}")
       ^/^ Prog.pretty prog ^/^ !^"{"
       ^^ align (Prop.pretty post ^^ !^"}")))
