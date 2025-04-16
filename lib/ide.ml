open Base

type t = string * int [@@deriving show]

let index_table = Hashtbl.create (module String)

let int_to_subscript n =
  let rec aux n s =
    let s =
      (match n % 10 with
      | 0 -> "₀"
      | 1 -> "₁"
      | 2 -> "₂"
      | 3 -> "₃"
      | 4 -> "₄"
      | 5 -> "₅"
      | 6 -> "₆"
      | 7 -> "₇"
      | 8 -> "₈"
      | 9 -> "₉"
      | _ -> failwith "unreachable")
      ^ s
    in
    if n > 9 then aux (n / 10) s else s
  in
  (if n < 0 then "₋" else "") ^ aux n ""

let to_string (s, i) = if i = 0 then s else s ^ int_to_subscript i

let raw_of_string s =
  Hashtbl.update index_table s ~f:(function
    | None | Some 0 -> 0
    | _ ->
        failwith "can't use raw_of_string on an identifier with fresh versions");
  (s, 0)

let fresh_of_string s =
  ( s,
    Hashtbl.update_and_return index_table s
      ~f:(Option.value_map ~default:1 ~f:(( + ) 1)) )

let fresh_of_t (s, _) = fresh_of_string s
let equal (s, i) (t, j) = String.equal s t && i = j
let ( = ) = equal
