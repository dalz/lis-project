open Base

type t = Ide.t * int [@@deriving show]

let index_table = Hashtbl.create (module Ide)

let fresh_of_ide x =
  ( x,
    Hashtbl.update_and_return index_table x
      ~f:(Option.value_map ~default:1 ~f:(( + ) 1)) )

let int_to_superscript n =
  let rec aux n s =
    let s =
      (match n % 10 with
      | 0 -> "⁰"
      | 1 -> "¹"
      | 2 -> "²"
      | 3 -> "³"
      | 4 -> "⁴"
      | 5 -> "⁵"
      | 6 -> "⁶"
      | 7 -> "⁷"
      | 8 -> "⁸"
      | 9 -> "⁹"
      | _ -> failwith "unreachable")
      ^ s
    in
    if n > 9 then aux (n / 10) s else s
  in
  (if n < 0 then "₋" else "") ^ aux n ""

let to_string (x, n) = Ide.to_string x ^ int_to_superscript n
