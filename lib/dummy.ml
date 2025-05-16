open Base

module T = struct
  (* when int = 0, represents a program variable (Ide.t) *)
  type t = Ide.t * int [@@deriving show, equal, compare, sexp_of]

  let index_table = Hashtbl.create (module Ide)

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

  let to_string (x, n) =
    Ide.to_string x ^ if n = 0 then "" else int_to_superscript n

  let raw s n =
    if n < 0 then failwith "dummy variable with negative ticks"
    else
      let x = Ide.raw_of_string s in
      Hashtbl.update index_table x ~f:(function
        | Some m when n < m -> m
        | _ -> n);
      (x, n)

  let raw_of_ide x =
    Ide.assert_raw x;
    (x, 0)

  let fresh_of_ide x =
    ( x,
      Hashtbl.update_and_return index_table x
        ~f:(Option.value_map ~default:1 ~f:(( + ) 1)) )

  let fresh_of_t (x, n) =
    if n = 0 then (Ide.fresh_of_t x, 0) else failwith "TODO decide what to do"
  (* Ide.fresh_of_t x |> fresh_of_ide *)

  let is_ide (_, n) = n = 0
  let get_ide (x, _) = x
end

include T
include Comparator.Make (T)
