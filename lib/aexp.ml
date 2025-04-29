open Base
open PPrint

type bop = Sum | Sub | Mul | Div | Mod [@@deriving show]
type uop = Neg [@@deriving show]

type t = Num of int | Var of Ide.t | Bop of bop * t * t | Uop of uop * t
[@@deriving show]

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
  | Var iden -> Var (if Ide.(iden = id) then id1 else iden)
  | Bop (op, ae1, ae2) -> Bop (op, subst ae1 id id1, subst ae2 id id1)
  | Uop (op, ae1) -> Uop (op, subst ae1 id id1)

let bop_to_string = function
  | Sum -> "+"
  | Sub -> "-"
  | Mul -> "*"
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
    | Var x -> !^(Ide.to_string x)
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
