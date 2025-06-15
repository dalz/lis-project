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

let iter_rule exec ( let* ) s p =
  let* s' = exec s p in
  let s'' = Executor_state.abstract_join s s' in
  let* s''' = exec s'' p in
  [ Ok (Executor_state.abstract_join ~ensure_equal:true s'' s''') ]

let reduce_until_exn ~f = function
  | x :: y :: zs -> (
      match f x y with
      | Container.Continue_or_stop.Continue init ->
          List.fold_until ~init ~finish:Fn.id ~f zs
      | Stop res -> res)
  | [ x ] -> x
  | [] -> failwith "reduce_until_exn applied to empty list"

let exec ?(denoise = true) ~on_step s p =
  match
    Executor.exec { bind; on_step; alloc_rule; choice_rule; iter_rule } s p
    |> List.map ~f:(function
         | Ok s ->
             Ok
               (let post =
                  if denoise then Executor_state.dummy_dismantler s else s
                in
                Executor_state.to_prop post)
         | (Err _ | Stuck _) as s -> s)
  with
  | [] -> []
  | ps ->
      [
        (match
           reduce_until_exn ps ~f:(fun acc s ->
               match (acc, s) with
               | Ok p, Ok q -> Continue (Ok (Prop.Or (p, q)))
               | _, (Err _ | Stuck _) -> Stop s
               | (Err _ | Stuck _), _ -> failwith "unreachable")
         with
        | Ok p -> Ok (Prop.simpl p)
        | s -> s);
      ]
