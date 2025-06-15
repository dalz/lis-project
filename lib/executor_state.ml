open Base
open PPrint

type heapval =
  | Val of Aexp.t (* x ↦ e *)
  | Undefined (* x ↦ _ *)
  | Dealloc (* x !↦ *)
[@@deriving equal]

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
    | PointsToUndefined x -> dummify_var map x (fun x' -> PointsToUndefined x')
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
               | (PointsTo _ | PointsToNothing _ | PointsToUndefined _ | Emp) as
                 a ->
                   Either.First a
               | Bool e -> Either.Second e))

let build_heap chunks =
  List.filter_map chunks
    ~f:
      Atom.(
        function
        | PointsTo (x, e) -> Some (`PointsTo (x, e))
        | PointsToNothing x -> Some (`PointsToNothing x)
        | PointsToUndefined x -> Some (`PointsToUndefined x)
        | Emp -> None
        | Bool _ -> failwith "unreachable")
  |> List.fold
       ~init:(Map.empty (module Dummy), [])
       ~f:(fun
           (map, p)
           ((`PointsTo (x, _) | `PointsToNothing x | `PointsToUndefined x) as
            pto)
         ->
         let data =
           match pto with
           | `PointsTo (_, e) -> Val e
           | `PointsToNothing _ -> Dealloc
           | `PointsToUndefined _ -> Undefined
         in
         match Map.add map ~key:x ~data with
         | `Ok map -> (map, p)
         | `Duplicate -> (
             match (Map.find_exn map x, pto) with
             | Val a, `PointsTo (_, b) -> (map, Path_cond.Cmp (Eq, a, b) :: p)
             | Undefined, `PointsTo _ -> (Map.set map ~key:x ~data, p)
             | (Val _ | Undefined), `PointsToUndefined _ -> (map, p)
             | _ -> (map, [ Const false ])))

let add_bexp_to_path_cond { dummies; heap; path_cond } b =
  let dummies, b = dummify_bexp dummies b in
  let Norm_bexp.{ always; alts } = Norm_bexp.of_bexp b in
  List.map alts ~f:(fun pc ->
      { dummies; heap; path_cond = path_cond @ always @ pc })

let list_of_norm_prop (_, Norm_prop.Disj ds) =
  let dummies, ds = dummify_ds ds in
  let aux (chunks, bool_preds) : t list =
    let heap, path_cond = build_heap chunks in
    List.reduce bool_preds ~f:(fun a b -> Bexp.Bop (And, a, b))
    |> Option.value ~default:(Bexp.Const true)
    |> add_bexp_to_path_cond { dummies; heap; path_cond }
  in
  extract_chunks ds |> List.concat_map ~f:aux

let subst { dummies; heap; path_cond } x y =
  let f z = if Dummy.equal x z then y else z in
  match
    Map.map heap ~f:(function
      | Val a -> Val (Aexp.subst a x (Var y))
      | (Undefined | Dealloc) as z -> z)
    |> Map.map_keys (module Dummy) ~f
  with
  | `Ok heap ->
      Some
        {
          dummies = Map.map dummies ~f;
          heap;
          path_cond = Path_cond.subst path_cond x (Var y);
        }
  | `Duplicate_key _ -> None

let simpl s =
  let aux ({ path_cond; _ } as s) =
    let path_cond = Path_cond.simpl path_cond in
    Path_cond.get_substs path_cond
    |> List.fold_result ~init:{ s with path_cond }
         ~f:(fun ({ path_cond; _ } as s) (x, a) ->
           match a with
           | Aexp.Var y -> subst s x y |> Result.of_option ~error:()
           | _ ->
               Ok
                 {
                   s with
                   path_cond = Path_cond.subst ~noloop:true path_cond x a;
                 })
  in
  let rec fix f ({ path_cond = pc; _ } as s) =
    Result.bind (f s) ~f:(fun ({ path_cond = pc'; _ } as s') ->
        if Path_cond.equal pc pc' then Ok s' else fix f s')
  in
  match fix aux s with Ok s -> Some s | Error () -> None

let to_prop { dummies; heap; path_cond } =
  let open Prop in
  let dummies =
    Map.to_alist dummies
    |> List.map ~f:(fun (x, x') ->
           Atom (Bool (Cmp (Eq, Var (Dummy.raw_of_ide ~force:true x), Var x'))))
    |> List.reduce ~f:(fun prop eq -> And (prop, eq))
    |> Option.value ~default:(Atom (Bool (Const true)))
  in
  let heap =
    Map.to_alist heap
    |> List.map ~f:(fun (x', v) ->
           Atom
             (match v with
             | Val a -> PointsTo (x', a)
             | Dealloc -> PointsToNothing x'
             | Undefined -> PointsToUndefined x'))
    |> List.reduce ~f:(fun acc pto -> Sep (acc, pto))
    |> Option.value ~default:(Atom Emp)
  in
  let path_cond =
    List.fold path_cond ~init:(Atom (Bool (Const true))) ~f:(fun acc a ->
        And
          ( acc,
            match a with
            | Path_cond.Const b -> Atom (Bool (Const b))
            | Cmp (op, a, b) -> Atom (Bool (Cmp (op, a, b))) ))
  in
  And (dummies, And (heap, path_cond))

(*
let bound_to_string b =
  (match b with
  | `Eq _ -> "="
  | `Ge _ -> ">="
  | `Le _ -> "<="
  | `Any -> "any"
  | `Unknown -> "unknown")
  ^
  match b with
  | `Eq n | `Ge n | `Le n -> " " ^ Int.to_string n
  | `Any | `Unknown -> ""

let print_bounds bs =
  Stdio.Out_channel.print_endline "[";
  Map.iteri bs ~f:(fun ~key:x ~data:b ->
      Stdio.Out_channel.print_endline
        ("  " ^ Dummy.to_string x ^ " " ^ bound_to_string b));
  Stdio.Out_channel.print_endline "]"
*)

let abstract_join_path_cond ~ensure_equal s t =
  let join_bounds ~ignore_unknowns b1 b2 =
    match (b1, b2) with
    | `Eq n, `Eq m -> if n = m then `Eq n else if n < m then `Ge n else `Le m
    | (`Le n | `Eq n), (`Le m | `Eq m) -> `Le (Int.max n m)
    | (`Ge n | `Eq n), (`Ge m | `Eq m) -> `Ge (Int.min n m)
    | `Unknown, _ when ignore_unknowns -> b2
    | _ -> `Any
  in
  let abstract_pc { dummies; path_cond = p; _ } =
    let rec eval bs = function
      | Aexp.Num n -> `Eq n
      | Var x -> Map.find bs x |> Option.value ~default:`Unknown
      | Uop (Neg, a) -> (
          match eval bs a with
          | `Eq n -> `Eq (-n)
          | `Ge n -> `Le (-n)
          | `Le n -> `Ge (-n)
          | (`Any | `Unknown) as a -> a)
      | Bop (op, a, b) -> (
          match (op, eval bs a, eval bs b) with
          | Sum, `Eq n, `Eq m -> `Eq (n + m)
          | Sub, `Eq n, `Eq m -> `Eq (n - m)
          | Sum, (`Ge n | `Eq n), (`Ge m | `Eq m) -> `Ge (Int.min n m)
          | Sub, (`Le n | `Eq n), (`Le m | `Eq m) -> `Le (Int.max n (-m))
          | Sum, (`Le n | `Eq n), `Le m when m <= 0 -> `Le n
          | Sub, (`Le n | `Eq n), `Ge m when m >= 0 -> `Le n
          | Sub, `Ge n, `Le m when m <= 0 -> `Ge n
          | _, `Unknown, _ | _, _, `Unknown -> `Unknown
          | _ -> `Any)
    in
    let update_bounds bs =
      let compute_bound flip bs progress (op : Cmp.t) x e =
        (*
        Stdio.Out_channel.print_endline
          (Bool.to_string flip ^ " " ^ Cmp.to_string op ^ " "
         ^ Dummy.to_string x ^ " " ^ Aexp.show e);
         print_bounds bs;
         *)
        let old = Map.find bs x in
        let new_ =
          match (flip, op, eval bs e) with
          | _, _, ((`Any | `Unknown) as b) -> b
          | _, Eq, b -> b
          | _, Ne, _ -> `Any
          | false, (Le | Lt), (`Le n | `Eq n) -> `Le n
          | false, (Le | Lt), `Ge _ -> `Any
          | true, (Le | Lt), (`Ge n | `Eq n) -> `Ge n
          | true, (Le | Lt), `Le _ -> `Any
        in
        let bs =
          Map.set bs ~key:x
            ~data:
              (join_bounds ~ignore_unknowns:true
                 (Option.value ~default:`Unknown old)
                 new_)
        in
        ( bs,
          match (old, new_) with
          | Some `Unknown, `Unknown -> progress
          | None, _ | Some `Unknown, _ -> true
          | _ -> progress )
      in
      List.fold p ~init:(bs, false) ~f:(fun (bs, progress) -> function
        | Path_cond.Cmp (op, Var x, e) -> compute_bound false bs progress op x e
        | Cmp (op, e, Var x) -> compute_bound true bs progress op x e
        | Cmp (op, Uop (Neg, Var x), e) ->
            compute_bound true bs progress op x (Aexp.simpl (Uop (Neg, e)))
        | Cmp _ | Const _ -> (bs, progress))
    in
    let rec fix f x =
      let y, progress = f x in
      if progress then fix f y else y
    in
    fix update_bounds (Map.empty (module Dummy))
    |> Map.filteri ~f:(fun ~key:x' ~data:_ ->
           match Map.find dummies (Dummy.get_ide x') with
           | Some x'' -> Dummy.equal x' x''
           | None -> false)
  in
  let dedummify_abstract r =
    match Map.map_keys (module Ide) (abstract_pc r) ~f:Dummy.get_ide with
    | `Ok m -> m
    | `Duplicate_key _ -> failwith "should have been removed by abstract_pc"
  in
  let bs1 = dedummify_abstract s in
  let bs2 = dedummify_abstract t in
  Map.fold_until bs1 ~init:[] ~finish:Fn.id ~f:(fun ~key:x ~data:b1 p ->
      match Map.find bs2 x with
      | Some b2 ->
          let x' = Map.find_exn s.dummies x in
          if (not ensure_equal) || Poly.equal b1 b2 then
            match join_bounds ~ignore_unknowns:false b1 b2 with
            | `Eq n -> Continue (Path_cond.Cmp (Eq, Var x', Num n) :: p)
            | `Le n -> Continue (Cmp (Le, Var x', Num n) :: p)
            | `Ge n -> Continue (Cmp (Le, Num n, Var x') :: p)
            | _ -> Continue p
          else Stop []
      | None -> Continue p)

(** Removes noises in the postcondition*)
let dummy_dismantler (post : t) : t =
  (* Creates the set of all dummy variables available *)
  let dummy_set =
    Base.Map.fold post.dummies
      ~init:(Set.empty (module Dummy))
      ~f:(fun ~key:_ ~data acc -> Set.add acc data)
  in

  (* for each atom in path_condition:
      - if atom has at least a dummy in S or no dummy, remove it from 
        path_condition and move it in another list. 
        All dummies of atom must be added on S .
      - otherwise don't do nothing with atom.
      *)
  let module Result = struct
    type t = {
      set : (Dummy.t, Dummy.comparator_witness) Base.Set.t;
      path_cond : Path_cond.t;
      matched_atoms : Path_cond.t;
    }
  end in
  let scan_path_condition (r : Result.t) : Result.t =
    List.fold r.path_cond
      ~init:Result.{ r with path_cond = [] }
      ~f:(fun acc atom ->
        match atom with
        | Const _ ->
            (* It has no dummies, add atom to matched_atoms *)
            { acc with path_cond = atom :: acc.matched_atoms }
        | Cmp (_, a1, a2) ->
            (* - If at least a1 or a2 is a dummy in S -> 
                      add atom to matched_atoms 
                      && add all dummies of atom in S
               - Otherwise put atom in path_cond *)
            let check_dummy (a : Aexp.t) status () =
              let is_found, set = status in
              match a with
              | Var dummy ->
                  if is_found then (true, Set.add set dummy)
                  else if Set.mem set dummy then (true, set)
                  else (false, set)
              | _ -> (is_found, set)
            in
            let status' = check_dummy a1 (false, acc.set) () in
            let found'', set'' = check_dummy a2 status' () in
            if found'' then
              {
                acc with
                set = set'';
                matched_atoms = atom :: acc.matched_atoms;
              }
            else { acc with path_cond = atom :: acc.path_cond })
  in
  (* Repeat scan_path_condition until the returned set equals the one gave in 
     input. When exiting return only the resulting matched_atoms *)
  let rec solution (r : Result.t) =
    let r' = scan_path_condition r in
    if Set.equal r.set r'.set then { post with path_cond = r'.matched_atoms }
    else solution r'
  in
  solution
    Result.{ set = dummy_set; path_cond = post.path_cond; matched_atoms = [] }

let heap_equal_up_to_dummies s t =
  let filter_heap u =
    Map.fold u.dummies ~init:[] ~f:(fun ~key:_ ~data:x' h ->
        match Map.find u.heap x' with Some v -> (x', v) :: h | None -> h)
  in
  let s_heap = filter_heap s in
  let t_heap = filter_heap t in
  not
    (match List.zip s_heap t_heap with
    | Ok hs ->
        List.exists hs ~f:(fun ((x', v), (y', u)) ->
            not
              (Ide.equal (Dummy.get_ide x') (Dummy.get_ide y')
              && equal_heapval v u))
    | Unequal_lengths -> true)

let abstract_join ?(ensure_equal = false) s t =
  if heap_equal_up_to_dummies s t then
    { s with path_cond = abstract_join_path_cond ~ensure_equal s t }
  else
    {
      dummies = Map.empty (module Ide);
      heap = Map.empty (module Dummy);
      path_cond = [];
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
