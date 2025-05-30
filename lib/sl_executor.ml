open Base
open Executor

let bind x f =
  match x with Ok s -> f s | Err s -> [ Stuck s ] | Stuck _ -> [ x ]

let alloc_rule s x =
  let x' = Dummy.fresh_of_ide x in
  Ok
    Executor_state.
      {
        s with
        heap = Map.set s.heap ~key:x' ~data:Undefined;
        dummies = Map.set s.dummies ~key:x ~data:x';
      }

let choice_rule s p1 p2 = [ (s, p1); (s, p2) ]

let exec ~on_step s p =
  Executor.exec { bind; on_step; alloc_rule; choice_rule } s p
  |> List.fold_until ~init:(Prop.Atom (Bool (Const false)))
       ~finish:(fun s -> Ok (Simplify.simplify_prop s))
       ~f:(fun acc -> function
         | Ok s -> Continue (Prop.Or (acc, Executor_state.to_prop s))
         | (Err _ | Stuck _) as s -> Stop s)
  |> List.return
