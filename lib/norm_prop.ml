open Base
open PPrint

type conj = Conj of Atom.t list [@@deriving show, compare]
type sepj = Sepj of conj list [@@deriving show, compare]
type disj = Disj of sepj list [@@deriving show]
type t = Dummy.t list * disj [@@deriving show]

(* converts the assertions to ∃-DNF:
 * ∃ (x_1 ... x_n) . (⋁_i q_i)
 * with q_i composed of ∧ and * only *)

let or_ p q : Prop.t = Or (p, q)
let and_ p q : Prop.t = And (p, q)
let sep p q : Prop.t = Sep (p, q)

(* returns existential variables, used variables, prop without exists *)
let rec extract_exists (exs : Dummy.t list) (uxs : Dummy.t list) (p : Prop.t) =
  let aux p q op =
    let exs, uxs, p = extract_exists exs uxs p in
    let exs, uxs, q = extract_exists exs uxs q in
    (exs, uxs, op p q)
  in
  match p with
  | Exists (x, p) ->
      let x, p =
        if List.mem uxs x ~equal:Dummy.equal then
          let x' = Dummy.fresh_of_t x in
          (x', Prop.subst p x x')
        else (x, p)
      in
      extract_exists (x :: exs) (x :: uxs) p
  | Atom a -> (exs, uxs @ Atom.fv a, p)
  | And (p, q) -> aux p q and_
  | Or (p, q) -> aux p q or_
  | Sep (p, q) -> aux p q sep

(* Performs one normalization step on a prop that doesn't contain exists.
   Returns the new prop and a boolean, which is false if nothing changed. *)
let rec normalize_step (p : Prop.t) : Prop.t * bool =
  let left p _ = p in
  let right _ q = q in
  let distribute dir op1 op2 p q r =
    let a, _ = normalize_step (op1 p (dir q r)) in
    let b, _ = normalize_step (op1 (dir p q) r) in
    (op2 a b, true)
  in
  let visit op p q =
    let a, cha = normalize_step p in
    let b, chb = normalize_step q in
    (op a b, cha || chb)
  in

  match p with
  (* atoms *)
  | Atom _ -> (p, false)
  (* operator distributivity *)
  | And (p, Or (q, r)) -> distribute left and_ or_ p q r
  | And (Or (p, q), r) -> distribute right and_ or_ p q r
  | And (p, Sep (q, r)) -> distribute left and_ sep p q r
  | And (Sep (p, q), r) -> distribute right and_ sep p q r
  | Sep (p, Or (q, r)) -> distribute left sep or_ p q r
  | Sep (Or (p, q), r) -> distribute right sep or_ p q r
  (* if no rule matches: *)
  | And (p, q) -> visit and_ p q
  | Or (p, q) -> visit or_ p q
  | Sep (p, q) -> visit sep p q
  | Exists _ -> failwith "exists should have been removed by extract_exists"

(* Applies normalize_step until the Prop.t is normalized. *)
let rec normalize_iter (p : Prop.t) : Prop.t =
  let p, chng = normalize_step p in
  if chng then normalize_iter p else p

let of_prop (p : Prop.t) ~prog_vars : t =
  let rec aux_and (p : Prop.t) (Conj al) =
    match p with
    | Atom a -> Conj (a :: al)
    | And (p, q) -> aux_and p (Conj al) |> aux_and q
    | _ -> failwith "p is not in normal form AND"
  in
  let rec aux_sep (p : Prop.t) (Sepj al) =
    match p with
    | Sep (p, q) -> aux_sep p (Sepj al) |> aux_sep q
    | Atom _ | And _ -> Sepj (aux_and p (Conj []) :: al)
    | _ -> failwith "p is not in normal form SEP"
  in
  let rec aux_or (p : Prop.t) (Disj al) =
    match p with
    | Or (p, q) -> aux_or p (Disj al) |> aux_or q
    | Atom _ | Sep _ | And _ -> Disj (aux_sep p (Sepj []) :: al)
    | _ -> failwith "p is not in normal form OR"
  in
  let xs, _, p = extract_exists [] prog_vars p in
  (xs, aux_or (normalize_iter p) (Disj []))

let simpl_conj (Conj cs) =
  let cs =
    List.map cs ~f:Atom.simpl
    |> List.fold_until ~init:[] ~finish:Fn.id
         ~f:
           Atom.(
             fun cs -> function
               | Bool (Const true) | Emp -> Continue cs
               | Bool (Const false) as f -> Stop [ f ]
               | a -> Continue (a :: cs))
    |> List.stable_dedup ~compare:Atom.compare
    |> List.rev
  in
  let locs =
    List.fold cs
      ~init:(Set.empty (module Dummy))
      ~f:(fun locs -> function
        | PointsTo (x, _) | PointsToUndefined x | PointsToNothing x ->
            Set.add locs x
        | Emp | Bool _ -> locs)
  in
  (Conj cs, locs)

let simpl_sepj (Sepj ss) =
  Sepj
    (List.map ss ~f:simpl_conj
    |> List.fold_until
         ~init:([], Set.empty (module Dummy))
         ~finish:(fun (ss, _) -> ss)
         ~f:(fun (ss, locs) (Conj cs, locs') ->
           if Set.are_disjoint locs locs' then
             let locs = Set.union locs locs' in
             match cs with
             | [] (* true *) -> Continue (ss, locs)
             | [ Bool (Const false) ] -> Stop [ Conj cs ]
             | _ -> Continue (Conj cs :: ss, locs)
           else
             (* non-disjoint heaps *)
             Stop [ Conj [ Bool (Const false) ] ])
    |> List.stable_dedup ~compare:compare_conj
    |> List.rev)

let simpl (xs, Disj ds) =
  ( xs,
    Disj
      (List.map ds ~f:simpl_sepj
      |> List.fold_until ~init:[] ~finish:Fn.id ~f:(fun ds -> function
           | Sepj [] (* true *) -> Stop [ Sepj [] ]
           | Sepj [ Conj [ Bool (Const false) ] ] -> Continue ds
           | Sepj ss -> Continue (Sepj ss :: ds))
      |> List.stable_dedup ~compare:compare_sepj
      |> List.rev) )

let or_ (xs1, Disj ds1) (xs2, Disj ds2) =
  (List.stable_dedup (xs1 @ xs2) ~compare:Dummy.compare, Disj (ds1 @ ds2))

let aux_pretty op default f xs =
  if List.is_empty xs then utf8string default
  else
    align
      (group
         ((if List.length xs > 1 then ifflat empty (blank 2) else empty)
         ^^ separate_map (break 1 ^^ utf8string op ^^ space) f xs))

let conj_pretty (Conj xs) = aux_pretty "∧" "⊤" Atom.pretty xs
let sepj_pretty (Sepj xs) = aux_pretty "∗" "⊤" conj_pretty xs

let pretty (xs, Disj ys) =
  let d = aux_pretty "∨" "⊥" sepj_pretty ys in
  if List.is_empty xs then d
  else
    hang 2
      (utf8string "∃" ^^ space
      ^^ align (flow_map (break 1) (Fn.compose utf8string Dummy.to_string) xs)
      ^^ !^"," ^/^ d)
