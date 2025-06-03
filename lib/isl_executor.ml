open Base
open Executor

let bind x f =
  match x with
  | Ok s -> f s
  | Err s ->
      List.map (f s) ~f:(function Ok z -> Err z | (Err _ | Stuck _) as z -> z)
  | Stuck _ -> [ x ]

let alloc_rule s x =
  let open Executor_state in
  (* always try to reuse existing deallocated locations (Alloc2 rule)
     TODO other strategies? *)
  let x' =
    Map.to_alist s.heap
    |> List.find_map ~f:(function x', Dealloc -> Some x' | _ -> None)
    |> Option.value_or_thunk ~default:(fun () -> Dummy.fresh_of_ide x)
  in
  Ok
    {
      heap = Map.set s.heap ~key:x' ~data:Undefined;
      dummies = Map.set s.dummies ~key:x ~data:x';
      path_cond = Cmp (Ne, Var x', Num 0) :: s.path_cond;
    }

(* TODO interactive mode? *)
let choice_rule s p1 _p2 = [ (s, p1) ]

let exec ~on_step s p =
  Executor.exec { bind; on_step; alloc_rule; choice_rule } s p
  |> List.map ~f:(function
       | Ok s -> Ok (Executor_state.to_prop s)
       | Err s -> Err s
       | Stuck s -> Stuck s)
