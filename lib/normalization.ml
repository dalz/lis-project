open Ast

type conj = Conj of atom list [@@deriving show]
type sepj = Sepj of conj list [@@deriving show]
type disj = Disj of sepj list [@@deriving show]
type norm_prop = Ast.ide list * disj [@@deriving show]

let fresh_counter = ref 0

let gen_fresh_id =
  fresh_counter := !fresh_counter + 1;
  "id" ^ string_of_int !fresh_counter

(* substitutes every instance of identifier id with id1 in a *)
let rec subst_aexp (a : Ast.aexp) id id1 =
  match a with
  | Num _ -> a
  | Var iden -> Var (if iden = id then id1 else iden)
  | Bop (op, ae1, ae2) -> Bop (op, subst_aexp ae1 id id1, subst_aexp ae2 id id1)
  | Uop (op, ae1) -> Uop (op, subst_aexp ae1 id id1)

(* substitutes every instance of identifier id with id1 in b *)
let rec subst_bexp (b : Ast.bexp) id id1 =
  match b with
  | Bool _ -> b
  | Cmp (op, ae1, ae2) -> Cmp (op, subst_aexp ae1 id id1, subst_aexp ae2 id id1)
  | BAnd (b1, b2) -> BAnd (subst_bexp b1 id id1, subst_bexp b2 id id1)
  | BOr (b1, b2) -> BOr (subst_bexp b1 id id1, subst_bexp b2 id id1)
  | Not b1 -> Not (subst_bexp b1 id id1)

(* substitutes every instance of identifier id with id1 in p *)
let rec subst (p : Ast.pred) id id1 =
  match p with
  | Atom True | Atom False | Atom Emp -> p
  | And (p1, p2) -> And (subst p1 id id1, subst p2 id id1)
  | Or (p1, p2) -> Or (subst p1 id id1, subst p2 id id1)
  (* exists: substitute only if id is not shadowed by iden *)
  | Exists (iden, p1) -> Exists (iden, if iden = id then p1 else subst p1 id id1)
  | Atom (Bool b1) -> Atom (Bool (subst_bexp b1 id id1))
  | Atom (PointsTo (iden, a)) ->
      Atom (PointsTo ((if iden = id then id1 else iden), subst_aexp a id id1))
  | Atom (PointsToNothing iden) ->
      Atom (PointsToNothing (if iden = id then id1 else iden))
  | Sep (p1, p2) -> Sep (subst p1 id id1, subst p2 id id1)

(* converts the assertions to ∃-DNF:
 * ∃ (x_1 ... x_n) . (⋁_i q_i)
 * with q_i composed of ∧ and * only *)

(* TODO normalization of outermost constructor idk *)
let rec normalize_aux (p : Ast.pred) =
  match p with
  (* atoms *)
  | Atom _ -> p
  (* operator distributivity *)
  | And (p1, Or (p2, p3)) ->
      Or (normalize_aux (And (p1, p2)), normalize_aux (And (p1, p3)))
  | And (Or (p1, p2), p3) ->
      Or (normalize_aux (And (p1, p3)), normalize_aux (And (p2, p3)))
  | And (p1, Sep (p2, p3)) ->
      Sep (normalize_aux (And (p1, p2)), normalize_aux (And (p1, p3)))
  | And (Sep (p1, p2), p3) ->
      Sep (normalize_aux (And (p1, p3)), normalize_aux (And (p2, p3)))
  | Sep (p1, Or (p2, p3)) ->
      Or (normalize_aux (Sep (p1, p2)), normalize_aux (Sep (p1, p3)))
  | Sep (Or (p1, p2), p3) ->
      Or (normalize_aux (Sep (p1, p3)), normalize_aux (Sep (p2, p3)))
  (* quantified operators *)
  | Exists (iden, Or (p1, p2)) -> Exists (iden, normalize_aux (Or (p1, p2)))
  | Exists (iden, And (p1, p2)) -> Exists (iden, normalize_aux (And (p1, p2)))
  | Exists (iden, Sep (p1, p2)) -> Exists (iden, normalize_aux (Sep (p1, p2)))
  (* exists and ops *)
  | And (Exists (iden, p1), p2) ->
      let fresh_id = gen_fresh_id in
      Exists (fresh_id, And (subst p1 iden fresh_id, p2))
  | And (p1, Exists (iden, p2)) ->
      let fresh_id = gen_fresh_id in
      Exists (fresh_id, And (p1, subst p2 iden fresh_id))
  | Or (Exists (iden, p1), p2) ->
      let fresh_id = gen_fresh_id in
      Exists (fresh_id, Or (subst p1 iden fresh_id, p2))
  | Or (p1, Exists (iden, p2)) ->
      let fresh_id = gen_fresh_id in
      Exists (fresh_id, Or (p1, subst p2 iden fresh_id))
  | Sep (Exists (iden, p1), p2) ->
      let fresh_id = gen_fresh_id in
      Exists (fresh_id, Sep (subst p1 iden fresh_id, p2))
  | Sep (p1, Exists (iden, p2)) ->
      let fresh_id = gen_fresh_id in
      Exists (fresh_id, Sep (p1, subst p2 iden fresh_id))
  (* if no rule matches: *)
  | And (p1, p2) -> And (normalize_aux p1, normalize_aux p2)
  | Or (p1, p2) -> Or (normalize_aux p1, normalize_aux p2)
  | Sep (p1, p2) -> Sep (normalize_aux p1, normalize_aux p2)
  | Exists (iden, p1) -> Exists (iden, normalize_aux p1)
(*     | _ -> failwith "not yet implemented" *)

let normalize (p : Ast.pred) =
  let rec aux_and (p : Ast.pred) (Conj al : conj) =
    match p with
    | Atom a -> Conj (a :: al)
    | And (p, q) -> aux_and p (Conj al) |> aux_and q
    | _ -> failwith "p is not in normal form AND"
  in
  let rec aux_sep (p : Ast.pred) (Sepj al : sepj) =
    match p with
    | Sep (p, q) -> aux_sep p (Sepj al) |> aux_sep q
    | Atom _ | And _ -> Sepj (aux_and p (Conj []) :: al)
    | _ -> failwith "p is not in normal form SEP"
  in
  let rec aux_or (p : Ast.pred) (Disj al : disj) =
    match p with
    | Or (p, q) -> aux_or p (Disj al) |> aux_or q
    | Atom _ | Sep _ | And _ -> Disj (aux_sep p (Sepj []) :: al)
    | _ -> failwith "p is not in normal form OR"
  in
  let rec aux_exists (p : Ast.pred) (xs : ide list) =
    match p with
    | Exists (id, q) -> aux_exists q (id :: xs)
    | Atom _ | Sep _ | And _ | Or _ -> (xs, aux_or p (Disj []))
  in
  aux_exists (normalize_aux p) []
