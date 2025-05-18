open Base
open Executor

let bind x f =
  match x with Ok s -> f s | Err s -> Stuck s | Stuck _ | Unreachable -> x

let alloc_rule s x =
  let x' = Dummy.fresh_of_ide x in
  Ok
    Executor_state.
      {
        s with
        heap = Map.set s.heap ~key:x' ~data:Undefined;
        dummies = Map.set s.dummies ~key:x ~data:x';
      }

let exec ~on_step = Executor.exec { bind; on_step; alloc_rule }
