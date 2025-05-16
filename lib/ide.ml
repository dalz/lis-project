open Base

module T = struct
  type t = string * int [@@deriving show, equal, compare, sexp_of, hash]

  let index_table = Hashtbl.create (module String)
  let done_parsing = ref false
  let set_done_parsing () = done_parsing := true

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
    if !done_parsing then
      failwith "can't use Ide.raw_of_string after parsing ends"
    else (s, 0)

  let assert_raw (_, i) =
    if !done_parsing then failwith "can't use Ide.assert_raw after parsing ends"
    else if i <> 0 then failwith "expected raw identifier"

  let fresh_of_string s =
    done_parsing := true;
    ( s,
      Hashtbl.update_and_return index_table s
        ~f:(Option.value_map ~default:1 ~f:(( + ) 1)) )

  let fresh_of_t (s, _) = fresh_of_string s
end

include T
include Comparator.Make (T)
