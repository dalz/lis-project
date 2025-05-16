open Base
open PPrint

type t =
  | Skip
  | Assert of Bexp.t
  | Assign of Ide.t * Aexp.t
  | AssignFromRef of Ide.t * Ide.t
  | AssignToRef of Ide.t * Ide.t
  | Alloc of Ide.t
  | Free of Ide.t
  | Error
[@@deriving show]

let fv = function
  | Skip | Error -> ([], [])
  | Assert e -> ([], Bexp.fv e)
  | Assign (x, e) -> ([ x ], Aexp.fv e)
  | AssignFromRef (x, y) | AssignToRef (x, y) -> ([ x; y ], [])
  | Alloc x | Free x -> ([ x ], [])

let pretty =
  let aux a b = group (hang 2 (utf8string a ^/^ b)) in
  function
  | Skip -> !^"skip"
  | Assert e -> aux "assert" (Bexp.pretty e)
  | Assign (x, e) -> aux (Ide.to_string x ^ " ←") (Aexp.pretty e)
  | AssignFromRef (x, y) ->
      aux (Ide.to_string x ^ " ←") (utf8string ("[" ^ Ide.to_string y ^ "]"))
  | AssignToRef (x, y) ->
      aux ("[" ^ Ide.to_string x ^ "] ←") (utf8string (Ide.to_string y))
  | Alloc x -> aux "alloc" (utf8string (Ide.to_string x))
  | Free x -> aux "free" (utf8string (Ide.to_string x))
  | Error -> !^"error"
