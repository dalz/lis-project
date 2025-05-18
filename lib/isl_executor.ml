let bind x f =
  let open Executor in
  match x with
  | Ok s -> f s
  | Err s -> (
      match f s with Ok z -> Err z | (Err _ | Stuck _ | Unreachable) as z -> z)
  | Stuck _ | Unreachable -> x

let exec ~on_step = Executor.exec { bind; on_step }
