open Base
open Ast
open Bexp

type conj = Conj of Atom.t list [@@deriving show]
type sepj = Sepj of conj list [@@deriving show]
type disj = Disj of sepj list [@@deriving show]
type t = Ide.t list * disj [@@deriving show]

(* substitutes every instance of identifier id with id1 in p *)
let rec subst (p : prop) id id1 =
  match p with
  | Atom (Bool (Const true)) | Atom (Bool (Const false)) | Atom Emp -> p
  | And (p1, p2) -> And (subst p1 id id1, subst p2 id id1)
  | Or (p1, p2) -> Or (subst p1 id id1, subst p2 id id1)
  (* exists: substitute only if id is not shadowed by iden *)
  | Exists (iden, p1) ->
      Exists (iden, if Ide.(iden = id) then p1 else subst p1 id id1)
  | Atom (Bool b1) -> Atom (Bool (Bexp.subst b1 id id1))
  | Atom (PointsTo (iden, a)) ->
      Atom
        (PointsTo ((if Ide.(iden = id) then id1 else iden), Aexp.subst a id id1))
  | Atom (PointsToNothing iden) ->
      Atom (PointsToNothing (if Ide.(iden = id) then id1 else iden))
  | Sep (p1, p2) -> Sep (subst p1 id id1, subst p2 id id1)

(* converts the assertions to ∃-DNF:
 * ∃ (x_1 ... x_n) . (⋁_i q_i)
 * with q_i composed of ∧ and * only *)

let or_ p q : prop = Or (p, q)
let and_ p q : prop = And (p, q)
let sep p q = Sep (p, q)

(* returns existential variables, used variables, prop without exists *)
let rec extract_exists (exs : Ide.t list) (uxs : Ide.t list) (p : prop) =
  let aux p q op =
    let exs, uxs, p = extract_exists exs uxs p in
    let exs, uxs, q = extract_exists exs uxs q in
    (exs, uxs, op p q)
  in
  match p with
  | Exists (x, p) ->
      let x, p =
        if List.mem uxs x ~equal:Ide.equal then
          let x' = Ide.fresh_of_t x in
          (x', subst p x x')
        else (x, p)
      in
      extract_exists (x :: exs) (x :: uxs) p
  | Atom a -> (exs, uxs @ Atom.fv a, p)
  | And (p, q) -> aux p q and_
  | Or (p, q) -> aux p q or_
  | Sep (p, q) -> aux p q sep

(* Performs one normalization step on a prop that doesn't contain exists.
   Returns the new prop and a boolean, which is false if nothing changed. *)
let rec normalize_step (p : prop) : prop * bool =
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

(* Applies normalize_step until the prop is normalized. *)
let rec normalize_iter (p : prop) : prop =
  let p, chng = normalize_step p in
  if chng then normalize_iter p else p

let of_prop (p : prop) : t =
  let rec aux_and (p : prop) (Conj al) =
    match p with
    | Atom a -> Conj (a :: al)
    | And (p, q) -> aux_and p (Conj al) |> aux_and q
    | _ -> failwith "p is not in normal form AND"
  in
  let rec aux_sep (p : prop) (Sepj al) =
    match p with
    | Sep (p, q) -> aux_sep p (Sepj al) |> aux_sep q
    | Atom _ | And _ -> Sepj (aux_and p (Conj []) :: al)
    | _ -> failwith "p is not in normal form SEP"
  in
  let rec aux_or (p : prop) (Disj al) =
    match p with
    | Or (p, q) -> aux_or p (Disj al) |> aux_or q
    | Atom _ | Sep _ | And _ -> Disj (aux_sep p (Sepj []) :: al)
    | _ -> failwith "p is not in normal form OR"
  in
  let xs, _, p = extract_exists [] [] p in
  (xs, aux_or (normalize_iter p) (Disj []))
