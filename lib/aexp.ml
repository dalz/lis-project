open Base
open PPrint

type bop = Sum | Sub | Mul | Div | Mod [@@deriving show, equal]
type uop = Neg [@@deriving show, equal]

type t = Num of int | Var of Dummy.t | Bop of bop * t * t | Uop of uop * t
[@@deriving show, equal]

let fv e =
  let rec aux e xs =
    match e with
    | Var x -> x :: xs
    | Bop (_, e1, e2) -> aux e1 xs |> aux e2
    | Uop (_, e) -> aux e xs
    | Num _ -> xs
  in
  aux e []

let rec subst (a : t) id id1 =
  match a with
  | Num _ -> a
  | Var iden -> Var (if Dummy.equal iden id then id1 else iden)
  | Bop (op, ae1, ae2) -> Bop (op, subst ae1 id id1, subst ae2 id id1)
  | Uop (op, ae1) -> Uop (op, subst ae1 id id1)

let compute op n m =
  (match op with
  | Sum -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )
  | Mod -> ( % ))
    n m

let simpl a =
  let mulop op1 op2 = if equal_bop op1 op2 then Sum else Sub in
  let rec aux = function
    (* op with known operands *)
    | Bop (op, Num n, Num m) -> Num (compute op n m)
    | Uop (Neg, Num n) -> Num (-n)
    (* identity and absorption *)
    | Bop (Sub, Num 0, a) -> Uop (Neg, aux a)
    | Bop (Sum, a, Num 0)
    | Bop (Sum, Num 0, a)
    | Bop (Mul, a, Num 1)
    | Bop (Mul, Num 1, a)
    | Bop (Sub, a, Num 0)
    | Bop (Div, a, Num 1) ->
        aux a
    | Bop (Mul, _, Num 0)
    | Bop (Mul, Num 0, _)
    | Bop (Div, Num 0, _)
    | Bop (Mod, _, Num 1) ->
        Num 0
    | Bop (Sub, a, a') when equal a a' -> Num 0
    | Bop (Div, a, a') when equal a a' -> Num 1
    | Bop (Div, _, Num 0) -> failwith "division by zero"
    (* push down negation *)
    | Uop (Neg, Bop (((Sum | Sub) as op), a, b)) ->
        Bop (op, Uop (Neg, aux a), Uop (Neg, aux b))
    | Uop (Neg, Bop (((Mul | Div | Mod) as op), a, b)) ->
        Bop (op, aux a, Uop (Neg, aux b))
    (* eliminate negation *)
    | Bop (((Mul | Div) as op), Uop (Neg, a), Uop (Neg, b)) ->
        Bop (op, aux a, aux b)
    | Bop (((Sum | Sub) as op), (Uop (Neg, _) as a), Num n) ->
        (* avoid infinite loops -x+1 -> 1-x -> -x+1 -> ... *)
        Bop (op, aux a, Num n)
    | Bop (((Sum | Sub) as op), Uop (Neg, a), b)
    | Bop (((Sum | Sub) as op), b, Uop (Neg, a)) ->
        Bop (mulop Sub op, aux b, aux a)
    | Bop (((Sum | Sub) as op), a, Num n) when n < 0 ->
        Bop (mulop Sub op, aux a, Num (-n))
    (* push negation right (more chances of finding numbers) *)
    | Bop (((Mul | Div | Mod) as op), Uop (Neg, a), b) ->
        Bop (op, aux a, Uop (Neg, aux b))
    (* push numbers right *)
    | Bop (((Sum | Mul) as op), Num n, a) -> Bop (op, aux a, Num n)
    | Bop (Sub, Num n, a) -> Bop (Sum, Uop (Neg, aux a), Num n)
    (* push parentheses right *)
    | Bop (Mul, Bop (Mul, a, b), c) -> Bop (Mul, aux a, Bop (Mul, aux b, aux c))
    | Bop (((Sum | Sub) as op1), Bop (((Sum | Sub) as op2), a, b), c) ->
        Bop (op2, aux a, Bop (mulop op1 op2, aux b, aux c))
    (* multiplication is repeated addition *)
    | Bop (Sum, a, b) when equal a b -> Bop (Mul, a, Num 2)
    | Bop (((Sum | Sub) as op), a, Bop (Mul, a', b)) when equal a a' ->
        Bop (Mul, aux a, Bop (op, Num 1, aux b))
    | Bop (((Sum | Sub) as op), Bop (Mul, a', b), a) when equal a a' ->
        Bop (Mul, aux a, Bop (op, aux b, Num 1))
    | Bop (((Sum | Sub) as op), a, Bop (Mul, b, a')) when equal a a' ->
        Bop (Mul, aux a, Bop (op, Num 1, aux b))
    | Bop (((Sum | Sub) as op), Bop (Mul, b, a'), a) when equal a a' ->
        Bop (Mul, aux a, Bop (op, aux b, Num 1))
    (* special interaction with div and mul *)
    | Bop (Div, Bop (Mul, a, Num n), Num m) when n % m = 0 ->
        Bop (Mul, aux a, Num (n / m))
    (* nothing to do *)
    | Bop (op, a, b) -> Bop (op, aux a, aux b)
    | Uop (Neg, a) -> Uop (Neg, aux a)
    | (Num _ | Var _) as a -> a
  in
  let rec fix f x =
    let y = f x in
    (* pretty y |> PPrint.ToChannel.pretty 1. 60 Out_channel.stdout; *)
    (* Stdio.Out_channel.print_endline ""; *)
    if equal x y then x else fix f y
  in
  fix aux a

(* pretty printer *)

let bop_to_string = function
  | Sum -> "+"
  | Sub -> "-"
  | Mul -> "×"
  | Div -> "/"
  | Mod -> "%"

let uop_to_string Neg = "-"

type assoc = Left | Assoc

let prec = function Mul | Div | Mod -> 1 | Sum | Sub -> 0

let assoc = function
  | Sum -> Assoc
  | Sub -> Left
  | Mul -> Assoc
  | Div -> Left
  | Mod -> Left

let pretty e =
  let rec aux t = function
    | Num n -> !^(Int.to_string n)
    | Var x -> !^(Dummy.to_string x)
    | Bop (op, e1, e2) ->
        let t1 =
          prec op - 1
          (* match assoc op with Right -> 0 | _ -> *)
        in
        let t2 = prec op - match assoc op with Left -> 0 | _ -> 1 in
        let p = Pp_util.pp_bop (aux t1 e1) (bop_to_string op) (aux t2 e2) in
        if t >= prec op then parens p else p
    | Uop (op, e) -> !^(uop_to_string op) ^^ aux Int.max_value e
  in
  aux (-1) e
