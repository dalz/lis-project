open PPrint

type t =
  | Bool of Bexp.t
  | PointsTo of Dummy.t * Aexp.t
  | Emp
  | PointsToNothing of Dummy.t
[@@deriving show]

let fv = function
  | PointsTo (x, e) -> x :: Aexp.fv e
  | PointsToNothing x -> [ x ]
  | Bool e -> Bexp.fv e
  | Emp -> []

let pretty = function
  | PointsTo (x, e) ->
      !^(Dummy.to_string x) ^^ space ^^ utf8string "↦" ^^ space ^^ Aexp.pretty e
  | PointsToNothing x -> !^(Dummy.to_string x) ^^ space ^^ utf8string "↦̸"
  | Bool e -> Bexp.pretty e
  | Emp -> !^"emp"
