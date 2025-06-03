open Base
open Executor_state

type 'a status =
  | Ok of 'a
  | Err of Executor_state.t
  | Stuck of Executor_state.t

type config = {
  bind :
    Executor_state.t status ->
    (Executor_state.t -> Executor_state.t status list) ->
    Executor_state.t status list;
  on_step : Executor_state.t -> unit;
  alloc_rule : Executor_state.t -> Ide.t -> Executor_state.t status;
  choice_rule :
    Executor_state.t -> Prog.t -> Prog.t -> (Executor_state.t * Prog.t) list;
}

let assign s x e =
  let x' = Dummy.fresh_of_ide x in
  let dummies, e' = Executor_state.dummify_aexp s.dummies e in
  let e' = Aexp.simpl e' in
  (* x = x' ∧ x' = e *)
  Executor_state.add_bexp_to_path_cond
    { s with dummies = Map.set dummies ~key:x ~data:x' }
    (Cmp (Eq, Var x', e'))
  |> List.hd_exn

let store s x' v = { s with heap = Map.set s.heap ~key:x' ~data:v }

let exec_cmd ~alloc_rule s =
  let ( let* ) x f =
    match x with Ok y -> f y | (Err _ | Stuck _) as x -> [ x ]
  in
  let dummy_of x =
    match Map.find s.dummies x with Some x' -> Ok x' | None -> Stuck s
  in
  let heap_val x =
    match Map.find s.heap x with
    | Some (Val v) -> Ok v
    | Some Dealloc -> Err s
    | Some Undefined | None ->
        Stuck s (* TODO decide what to do with undefined *)
  in
  let heap_has x =
    match Map.find s.heap x with
    | Some (Val _ | Undefined) -> Ok ()
    | Some Dealloc -> Err s
    | None -> Stuck s
  in
  let open Cmd in
  function
  | Skip -> [ Ok s ]
  | Assert e ->
      List.map (Executor_state.add_bexp_to_path_cond s e) ~f:(fun s -> Ok s)
  | Assign (x, e) -> [ Ok (assign s x e) ]
  | AssignFromRef (x, y) ->
      (* if y has no dummy variable, it has never been used before
         (in the program or precondition) so it is not allocated *)
      let* y' = dummy_of y in
      if Path_cond.is_null s.path_cond y' then [ Err s ]
      else
        let* e = heap_val y' in
        [ Ok (assign s x e) ]
  | AssignToRef (x, y) ->
      let* x' = dummy_of x in
      if Path_cond.is_null s.path_cond x' then [ Err s ]
      else
        let v =
          match Map.find s.dummies y with
          | Some y' -> Aexp.Var y'
          (* uninitialized variables are 0 by default *)
          | None -> Aexp.Num 0
        in
        let* _ = heap_has x' in
        [ Ok (store s x' (Val v)) ]
  | Alloc x -> [ alloc_rule s x ]
  | Free x ->
      let* x' = dummy_of x in
      if Path_cond.is_null s.path_cond x' then [ Err s ]
      else
        let* _ = heap_has x' in
        [ Ok (store s x' Dealloc) ]
  | Error -> [ Err s ]

let rec exec cfg s p : Executor_state.t status list =
  let ( let* ) (ss : Executor_state.t status list) f =
    List.concat_map ss ~f:(fun s -> cfg.bind s f)
  in

  let open Prog in
  if Path_cond.is_false s.path_cond then []
  else
    let* s =
      match p with
      | Cmd c -> exec_cmd ~alloc_rule:cfg.alloc_rule s c
      | Seq (p1, p2) ->
          let* s = exec cfg s p1 in
          cfg.on_step s;
          exec cfg s p2
      | Choice (p1, p2) ->
          cfg.choice_rule s p1 p2
          |> List.concat_map ~f:(fun (s, p) -> exec cfg s p)
      | _ -> failwith "not implemented"
    in

    match Executor_state.simpl s with Some s -> [ Ok s ] | None -> []
