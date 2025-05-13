open Base
open PPrint

type heapval =
  | Val of Aexp.t (* x ↦ e *)
  | Undefined (* x ↦ _ *)
  | Dealloc (* x !↦ *)

type t = {
  exvars : Dummy.t list;
  dummies : (Ide.t, Dummy.t, Ide.comparator_witness) Map.t;
  heap : (Dummy.t, heapval, Dummy.comparator_witness) Map.t;
  path_cond : Bexp.t;
}

let dummify ds =
  let dummify_var map x f =
    if Dummy.is_ide x then
      let x = Dummy.get_ide x in
      match Map.find map x with
      | Some x' -> (map, f x')
      | None ->
          let x' = Dummy.fresh_of_ide x in
          (Map.add_exn map ~key:x ~data:x', f x')
    else (map, f x)
  in
  let rec dummify_aexp map =
    let open Aexp in
    function
    | Var x -> dummify_var map x (fun x' -> Var x')
    | Bop (op, e1, e2) ->
        let map, e1 = dummify_aexp map e1 in
        let map, e2 = dummify_aexp map e2 in
        (map, Bop (op, e1, e2))
    | Uop (op, e) ->
        let map, e = dummify_aexp map e in
        (map, Uop (op, e))
    | Num _ as e -> (map, e)
  in
  let rec dummify_bexp map =
    let open Bexp in
    function
    | Const _ as e -> (map, e)
    | Cmp (op, e1, e2) ->
        let map, e1 = dummify_aexp map e1 in
        let map, e2 = dummify_aexp map e2 in
        (map, Cmp (op, e1, e2))
    | Bop (op, e1, e2) ->
        let map, e1 = dummify_bexp map e1 in
        let map, e2 = dummify_bexp map e2 in
        (map, Bop (op, e1, e2))
    | Not e ->
        let map, e = dummify_bexp map e in
        (map, Not e)
  in
  let dummify_atom map =
    let open Atom in
    function
    | Bool e ->
        let map, e = dummify_bexp map e in
        (map, Bool e)
    | PointsTo (x, e) ->
        let map, e = dummify_aexp map e in
        dummify_var map x (fun x' -> PointsTo (x', e))
    | PointsToNothing x -> dummify_var map x (fun x' -> PointsToNothing x')
    | Emp -> (map, Emp)
  in
  let open Norm_prop in
  List.fold_map ds
    ~init:(Map.empty (module Ide))
    ~f:(fun map (Sepj ss) ->
      let map, ss =
        List.fold_map ss ~init:map ~f:(fun map (Conj as_) ->
            let map, as_ = List.fold_map as_ ~init:map ~f:dummify_atom in
            (map, Conj as_))
      in
      (map, Sepj ss))

let extract_chunks =
  List.map ~f:(fun (Norm_prop.Sepj ss) ->
      List.concat_map ss ~f:(fun (Norm_prop.Conj cs) -> cs)
      |> List.partition_map
           ~f:
             Atom.(
               function
               | (PointsTo _ | PointsToNothing _ | Emp) as a -> Either.First a
               | Bool e -> Either.Second e))

let build_heap chunks =
  List.filter_map chunks
    ~f:
      Atom.(
        function
        | PointsTo (x, e) -> Some (`PointsTo (x, e))
        | PointsToNothing x -> Some (`PointsToNothing x)
        | Emp -> None
        | Bool _ -> failwith "unreachable")
  |> List.fold
       ~init:(Map.empty (module Dummy))
       ~f:(fun map -> function
         | `PointsTo (x, e) -> (
             match Map.add map ~key:x ~data:(Val e) with
             | `Ok map -> map
             | `Duplicate -> failwith "TODO simplification should have noticed?"
             )
         | `PointsToNothing x -> (
             match Map.add map ~key:x ~data:Dealloc with
             | `Ok map -> map
             | `Duplicate -> failwith "TODO simplification should have noticed?"
             ))

let list_of_norm_prop (xs, Norm_prop.Disj ds) =
  let dummies, ds = dummify ds in
  let build_state (chunks, bool_preds) =
    {
      exvars = xs;
      dummies;
      heap = build_heap chunks;
      path_cond =
        (let open Bexp in
         List.fold ~init:(Const true) ~f:(fun a b -> Bop (And, a, b)) bool_preds);
    }
  in
  extract_chunks ds |> List.map ~f:build_state

let pretty { exvars; dummies; heap; path_cond } =
  utf8string "∃ "
  ^^ hang 4
       (separate_map (break 1)
          (fun x' -> utf8string (Dummy.to_string x'))
          exvars)
  ^^ !^"," ^^ hardline
  ^^ group
       (align
          (separate_map
             (space ^^ utf8string "∗" ^^ break 1)
             (fun (x', hv) ->
               utf8string (Dummy.to_string x')
               ^^ space
               ^^
               match hv with
               | Val e ->
                   group (utf8string "↦" ^^ break 1 ^^ align (Aexp.pretty e))
               | Undefined -> utf8string "↦ _"
               | Dealloc -> utf8string "↦̸")
             (Map.to_alist heap)))
  ^^ hardline
  ^^ hang 4
       (utf8string "∧ "
       ^^ separate_map
            (space ^^ utf8string "∧" ^^ break 1)
            (fun (x, x') ->
              utf8string (Ide.to_string x ^ " = " ^ Dummy.to_string x'))
            (Map.to_alist dummies))
  ^^ hardline ^^ utf8string "∧ " ^^ Bexp.pretty path_cond
