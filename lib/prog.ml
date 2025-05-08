open Base
open PPrint

type t = Cmd of Cmd.t | Seq of t * t | Choice of t * t | Star of t
[@@deriving show]

let pretty =
  let rec aux t = function
    | Cmd c -> Cmd.pretty c
    | Seq (p1, p2) ->
        let prec = 0 in
        let p =
          group (align (aux (prec - 1) p1 ^^ !^";" ^/^ aux (prec - 1) p2))
        in
        if t >= prec then parens p else p
    | Choice (p1, p2) -> Pp_util.pp_assoc_bop aux 1 t p1 "+" p2
    | Star p ->
        let prec = 2 in
        let p = aux (prec - 1) p ^^ utf8string "⋆" in
        if t >= prec then parens p else p
  in
  aux (-1)
