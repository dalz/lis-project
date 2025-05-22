open Base

type t = { always : Path_cond.t; alts : Path_cond.t list }
[@@deriving show, equal]

let rec normalize_step (p : Bexp.t) : Bexp.t * bool =
  let left p _ = p in
  let right _ q = q in
  let distribute_and_or dir p q r =
    let a, _ = normalize_step (Bop (And, p, dir q r)) in
    let b, _ = normalize_step (Bop (And, dir p q, r)) in
    Bexp.(Bop (Or, a, b), true)
  in
  match p with
  (* atoms *)
  | Const _ | Cmp _ -> (p, false)
  (* operator distributivity *)
  | Bop (And, p, Bop (Or, q, r)) -> distribute_and_or left p q r
  | Bop (And, Bop (Or, p, q), r) -> distribute_and_or right p q r
  | Not (Bop (op, p, q)) ->
      let p, _ = normalize_step p in
      let q, _ = normalize_step q in
      (Bop ((match op with And -> Or | Or -> And), Not p, Not q), true)
  | Not (Cmp (op, a, b)) ->
      ( Cmp ((match op with Lt -> Le | Le -> Lt | Eq -> Ne | Ne -> Eq), b, a),
        true )
  | Not (Not b) -> normalize_step b
  | Not (Const b) -> (Const (not b), false)
  (* if no rule matches: *)
  | Bop (op, p, q) ->
      let a, cha = normalize_step p in
      let b, chb = normalize_step q in
      (Bop (op, a, b), cha || chb)

let rec normalize_iter (p : Bexp.t) : Bexp.t =
  let p, chng = normalize_step p in
  if chng then normalize_iter p else p

let of_bexp (b : Bexp.t) : t =
  let rec aux_and : Bexp.t -> Path_cond.t = function
    | Cmp (op, a, b) -> [ Cmp (op, a, b) ]
    | Const b -> [ Const b ]
    | Bop (And, p, q) -> aux_and p @ aux_and q
    | Bop (Or, _, _) | Not _ -> failwith "unreachable"
  in
  let rec aux_or : Bexp.t -> Path_cond.t list = function
    | (Bop (And, _, _) | Cmp _ | Const _) as b -> [ aux_and b ]
    | Bop (Or, p, q) -> aux_or p @ aux_or q
    | Not _ -> failwith "unreachable"
  in
  let alts = normalize_iter b |> aux_or in
  { always = []; alts }
