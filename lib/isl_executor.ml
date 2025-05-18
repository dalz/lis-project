open Base
open Executor

let bind x f =
  match x with
  | Ok s -> f s
  | Err s -> (
      match f s with Ok z -> Err z | (Err _ | Stuck _ | Unreachable) as z -> z)
  | Stuck _ | Unreachable -> x

let alloc_rule s x =
  let open Executor_state in
  (* always try to reuse existing deallocated locations (Alloc2 rule) *)
  let x' =
    Map.to_alist s.heap
    |> List.find_map ~f:(function x', Dealloc -> Some x' | _ -> None)
    |> Option.value_or_thunk ~default:(fun () -> Dummy.fresh_of_ide x)
  in
  Ok
    {
      s with
      heap = Map.set s.heap ~key:x' ~data:Undefined;
      dummies = Map.set s.dummies ~key:x ~data:x';
    }

let exec ~on_step = Executor.exec { bind; on_step; alloc_rule }
