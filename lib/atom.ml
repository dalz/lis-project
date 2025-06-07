open PPrint

type t =
  | Bool of Bexp.t
  | Emp
  | PointsTo of Dummy.t * Aexp.t
  | PointsToNothing of Dummy.t
  | PointsToUndefined of Dummy.t
[@@deriving show, compare, equal]

let fv = function
  | PointsTo (x, e) -> x :: Aexp.fv e
  | PointsToNothing x | PointsToUndefined x -> [ x ]
  | Bool e -> Bexp.fv e
  | Emp -> []

let subst a x x' =
  match a with
  | Bool b -> Bool (Bexp.subst b x x')
  | PointsTo (y, a) ->
      PointsTo ((if Dummy.equal y x then x' else y), Aexp.subst a x (Var x'))
  | PointsToNothing y | PointsToUndefined y ->
      PointsToNothing (if Dummy.equal y x then x' else y)
  | Emp -> Emp

let simpl = function
  | Bool b -> Bool (Bexp.simpl b)
  | PointsTo (x, a) -> PointsTo (x, Aexp.simpl a)
  | (Emp | PointsToNothing _ | PointsToUndefined _) as a -> a

let pretty = function
  | PointsTo (x, e) ->
      !^(Dummy.to_string x) ^^ space ^^ utf8string "↦" ^^ space ^^ Aexp.pretty e
  | PointsToNothing x -> !^(Dummy.to_string x) ^^ space ^^ utf8string "↦̸"
  | PointsToUndefined x -> !^(Dummy.to_string x) ^^ space ^^ utf8string "↦ _"
  | Bool e -> Bexp.pretty e
  | Emp -> !^"emp"
