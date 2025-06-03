open Base
open PPrint

type atom = Const of bool | Cmp of Cmp.t * Aexp.t * Aexp.t
[@@deriving show, equal]

type t = atom list [@@deriving show, equal]

let subst ?(noloop = false) p x a =
  List.map p ~f:(function
    | Cmp (Eq, Var x', b) as c when Dummy.equal x x' ->
        if noloop && List.mem (Aexp.fv a) x ~equal:Dummy.equal then c
        else Cmp (Eq, Var x, Aexp.subst b x a)
    | Cmp (op, e1, e2) -> Cmp (op, Aexp.subst e1 x a, Aexp.subst e2 x a)
    | Const _ as b -> b)

let is_false = function [ Const false ] -> true | _ -> false

let simpl_cmp (op : Cmp.t) a b =
  match (op, Aexp.simpl (Bop (Sub, a, b))) with
  | Eq, _ when Aexp.equal a b -> Const true
  | Eq, Bop ((Sum | Sub), Uop (Neg, Var x), a) -> Cmp (op, Var x, a)
  | _, Bop (Sum, ((Var _ | Uop (Neg, Var _)) as x), a) ->
      Cmp (op, x, Aexp.simpl (Uop (Neg, a)))
  | _, Bop (Sub, ((Var _ | Uop (Neg, Var _)) as x), a) -> Cmp (op, x, a)
  | _, Num n -> Const (Cmp.compute op n 0)
  | _, a -> Cmp (op, a, Num 0)

let rec simpl_bounds =
  let comp = function
    | `Le -> ( <= )
    | `Lt -> ( < )
    | `Ge -> ( >= )
    | `Gt -> ( > )
    | `Eq -> ( = )
    | `Ne -> ( <> )
  in
  let check_pair_aux ((op1, b1) : _ * Aexp.t) ((op2, b2) : _ * Aexp.t) =
    match (op1, b1, op2, b2) with
    | `Eq, Num n, `Eq, Num m -> if n <> m then `False else `Keep
    | `Eq, Num n, ((`Lt | `Le | `Gt | `Ge) as op), Num m ->
        if comp op n m then `DiscardRight else `False
    | `Eq, a, (`Lt | `Gt | `Ne), b ->
        if Aexp.equal a b then `False else `DiscardRight
    | `Lt, Num n, (`Gt | `Ge), Num m -> if n <= m then `False else `Keep
    | `Lt, a, (`Gt | `Ge), b -> if Aexp.equal a b then `False else `Keep
    | `Le, Num n, `Gt, Num m -> if n <= m then `False else `Keep
    | `Le, Num n, `Ge, Num m -> if n < m then `False else `Keep
    | `Le, a, `Ge, b -> if Aexp.equal a b then `Replace (`Eq, a) else `Keep
    | ((`Lt | `Le | `Gt | `Ge) as op), Num n, op', Num m when Poly.equal op op'
      ->
        if comp op n m then `DiscardRight else `DiscardLeft
    | `Lt, Num n, (`Le as op), Num m | `Gt, Num n, (`Ge as op), Num m ->
        if comp op n m then `DiscardRight else `DiscardLeft
    | _ ->
        if Poly.equal op1 op2 && Aexp.equal b1 b2 then `DiscardRight else `Keep
  in
  let check_pair b1 b2 =
    match check_pair_aux b1 b2 with
    | `Keep -> (
        match check_pair_aux b2 b1 with
        | `DiscardLeft -> `DiscardRight
        | `DiscardRight -> `DiscardLeft
        | r -> r)
    | r -> r
  in
  function
  | [] -> Ok []
  | b :: bs ->
      List.fold_result bs ~init:([], false) ~f:(fun (bs, discard) b' ->
          match check_pair b b' with
          | `Keep -> Ok (b' :: bs, discard)
          | `DiscardLeft -> Ok (b' :: bs, true)
          | `DiscardRight -> Ok (bs, discard)
          | `Replace r -> Ok (r :: bs, true)
          | `False -> Error ())
      |> Result.bind ~f:(function
           | bs, true -> simpl_bounds bs
           | bs, false -> simpl_bounds bs |> Result.map ~f:(List.cons b))

let update_bounds bmap cmp =
  let ribalt = function
    | `Le -> `Ge
    | `Lt -> `Gt
    | `Ge -> `Le
    | `Gt -> `Lt
    | (`Eq | `Ne) as op -> op
  in
  let negate_flip_op neg flip (op : Cmp.t) =
    let op =
      match (neg, op) with
      | false, Le -> `Le
      | true, Le -> `Gt
      | false, Lt -> `Lt
      | true, Lt -> `Ge
      | false, Eq -> `Eq
      | true, Eq -> `Ne
      | false, Ne -> `Ne
      | true, Ne -> `Eq
    in
    if flip then ribalt op else op
  in

  let aux bmap cmp flip =
    (match cmp with
    | Cmp (op, Var x, b) -> Some (x, negate_flip_op false flip op, b)
    | Cmp (op, Uop (Neg, Var x), b) -> Some (x, negate_flip_op true flip op, b)
    | _ -> None)
    |> Option.map ~f:(fun (x, op, b) ->
           Map.find bmap x |> Option.value ~default:[]
           |> List.cons (op, b)
           |> simpl_bounds
           |> Result.map ~f:(fun bs -> Map.set bmap ~key:x ~data:bs))
    |> Option.value ~default:(Ok bmap)
  in
  match cmp with
  | Cmp (op, a, b) ->
      aux bmap (Cmp (op, a, b)) false
      |> Result.bind ~f:(fun bmap -> aux bmap (Cmp (op, b, a)) true)
  | _ -> Ok bmap

let simpl p =
  let p =
    List.fold_until p
      ~init:([], Map.empty (module Dummy))
      ~finish:(fun (pc, _) -> pc)
      ~f:(fun (pc, bmap) -> function
        | Const true -> Continue (pc, bmap)
        | Const false as a -> Stop [ a ]
        | Cmp (op, a, b) -> (
            let cmp = simpl_cmp op a b in
            match update_bounds bmap cmp with
            | Ok bmap -> Continue (cmp :: pc, bmap)
            | Error () -> Stop [ Const false ]))
    |> List.rev
  in
  p

let get_substs =
  List.filter_map ~f:(function
    | Cmp (Eq, Var x, a) ->
        if List.mem (Aexp.fv a) x ~equal:Dummy.equal then None else Some (x, a)
    | _ -> None)

let is_null p x = List.mem p (Cmp (Eq, Var x, Num 0)) ~equal:equal_atom

let pretty p =
  separate_map
    (break 1 ^^ utf8string "∧" ^^ space)
    (function
      | Const b -> Bexp.pretty (Const b)
      | Cmp (op, e1, e2) -> Bexp.pretty (Cmp (op, e1, e2)))
    p
