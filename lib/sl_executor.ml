let bind x f =
  let open Executor in
  match x with Ok s -> f s | Err s -> Stuck s | Stuck _ | Unreachable -> x

let exec ~on_step = Executor.exec { bind; on_step }
