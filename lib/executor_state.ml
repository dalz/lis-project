open Base
open PPrint

type heapval =
  | Val of Aexp.t (* x ↦ e *)
  | Undefined (* x ↦ _ *)
  | Dealloc (* x !↦ *)

type t = {
  dummies : (Ide.t, Dummy.t, Ide.comparator_witness) Map.t;
  heap : (Dummy.t, heapval, Dummy.comparator_witness) Map.t;
  path_cond : Path_cond.t;
}

let dummify_var map x f =
  if Dummy.is_ide x then
    let x = Dummy.get_ide x in
    match Map.find map x with
    | Some x' -> (map, f x')
    | None ->
        let x' = Dummy.fresh_of_ide x in
        (Map.add_exn map ~key:x ~data:x', f x')
  else (map, f x)

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

let dummify_ds ds =
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

let add_bexp_to_path_cond { dummies; heap; path_cond } b =
  let dummies, b = dummify_bexp dummies b in
  let Norm_bexp.{ always; alts } = Norm_bexp.of_bexp b in
  List.map alts ~f:(fun pc ->
      { dummies; heap; path_cond = path_cond @ always @ pc })

let list_of_norm_prop (_, Norm_prop.Disj ds) =
  let dummies, ds = dummify_ds ds in
  let aux (chunks, bool_preds) : t list =
    List.reduce bool_preds ~f:(fun a b -> Bexp.Bop (And, a, b))
    |> Option.value ~default:(Bexp.Const true)
    |> add_bexp_to_path_cond
         { dummies; heap = build_heap chunks; path_cond = [] }
  in
  extract_chunks ds |> List.concat_map ~f:aux

let subst { dummies; heap; path_cond } x y =
  let f z = if Dummy.equal x z then y else z in
  {
    dummies = Map.map dummies ~f;
    heap =
      Map.map heap ~f:(function
        | Val a -> Val (Aexp.subst a x y)
        | (Undefined | Dealloc) as z -> z);
    path_cond = Path_cond.subst path_cond x y;
  }

let pretty { dummies; heap; path_cond } =
  (* ^^ (if List.is_empty exvars then empty *)
  (*     else *)
  (*       utf8string "∃ " *)
  (*       ^^ hang 4 *)
  (*            (separate_map (break 1) *)
  (*               (fun x' -> utf8string (Dummy.to_string x')) *)
  (*               exvars) *)
  (*       ^^ !^"," ^^ hardline) *)
  (if Map.is_empty heap then empty
   else
     group
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
     ^^ hardline ^^ utf8string "∧ ")
  ^^ (if Map.is_empty dummies then empty
      else
        hang 2
          (separate_map
             (break 1 ^^ utf8string "∧" ^^ space)
             (fun (x, x') ->
               utf8string (Ide.to_string x ^ " = " ^ Dummy.to_string x'))
             (Map.to_alist dummies))
        ^^ hardline ^^ utf8string "∧ ")
  ^^ Path_cond.pretty path_cond ^^ hardline
  ^^ !^"-------------------------"
