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

let reduce_until ~default ~f = function
  | x :: y :: zs -> (
      match f x y with
      | Container.Continue_or_stop.Continue init ->
          List.fold_until ~init ~finish:Fn.id ~f zs
      | Stop res -> res)
  | [ x ] -> x
  | [] -> default

let exec ~on_step s p =
  Executor.exec { bind; on_step; alloc_rule; choice_rule } s p
  |> List.map ~f:(function
       | Ok s -> Ok (Executor_state.to_prop s)
       | (Err _ | Stuck _) as s -> s)
  |> reduce_until ~default:(Ok (Prop.Atom (Bool (Const true)))) ~f:(fun acc s ->
         match (acc, s) with
         | Ok p, Ok q -> Continue (Ok (Prop.Or (p, q)))
         | _, (Err _ | Stuck _) -> Stop s
         | (Err _ | Stuck _), _ -> failwith "unreachable")
  |> ( function Ok p -> Ok (Prop.simpl p) | s -> s )
  (* List.fold_until ~init:([], Norm_prop.Disj []) *)
  (*    ~finish:(fun p -> Ok (Norm_prop.simpl p)) *)
  (*    ~f:(fun acc -> function *)
  (*      | Ok s -> Continue (Norm_prop.or_ acc (Executor_state.to_norm_prop s)) *)
  (*      | (Err _ | Stuck _) as s -> Stop s) *)
  |> List.return
