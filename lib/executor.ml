open Base
open Executor_state

(* TODO *)
let simplify s = { s with path_cond = Simplify.simplify_b s.path_cond }

type 'a status =
  | Ok of 'a
  | Err of Executor_state.t
  | Stuck of Executor_state.t
  | Unreachable

type config = {
  bind :
    Executor_state.t status ->
    (Executor_state.t -> Executor_state.t status) ->
    Executor_state.t status;
  on_step : Executor_state.t -> unit;
}

let add_path_cond s e =
  let dummies, e' = Executor_state.dummify_bexp s.dummies e in
  Bexp.{ s with dummies; path_cond = Bop (And, s.path_cond, e') }

let assign s x e =
  let x' = Dummy.fresh_of_ide x in
  let dummies, e' = Executor_state.dummify_aexp s.dummies e in
  let e' = Aexp.simpl e' in
  {
    s with
    (* x = x' ∧ x' = e *)
    dummies = Map.set dummies ~key:x ~data:x';
    path_cond = Bexp.(Bop (And, s.path_cond, Cmp (Eq, Var x', e')));
  }

let store s x' v = { s with heap = Map.set s.heap ~key:x' ~data:v }

let exec_cmd s =
  let ( let* ) x f =
    match x with Ok y -> f y | (Err _ | Stuck _ | Unreachable) as x -> x
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
  | Skip -> Ok s
  | Assert e -> Ok (add_path_cond s e)
  | Assign (x, e) -> Ok (assign s x e)
  | AssignFromRef (x, y) ->
      (* if y has no dummy variable, it has never been used before
         (in the program or precondition) so it is not allocated *)
      let* y' = dummy_of y in
      (* TODO y' null *)
      let* e = heap_val y' in
      Ok (assign s x e)
  | AssignToRef (x, y) ->
      let* x' = dummy_of x in
      let v =
        match Map.find s.dummies y with
        | Some y' -> Aexp.Var y'
        (* uninitialized variables are 0 by default (TODO) *)
        | None -> Aexp.Num 0
      in
      let* _ = heap_has x' in
      Ok (store s x' (Val v))
  | Alloc x ->
      (* TODO Alloc2 ISL *)
      let x' = Dummy.fresh_of_ide x in
      Ok
        (store
           { s with dummies = Map.set s.dummies ~key:x ~data:x' }
           x' Undefined)
  | Free x ->
      let* x' = dummy_of x in
      (* TODO null *)
      let* _ = heap_has x' in
      Ok (store s x' Dealloc)
  | Error -> Err s

let rec exec ({ bind = ( let* ); on_step } as cfg) s p =
  let open Prog in
  if Bexp.(equal s.path_cond (Const false)) then Unreachable
  else
    let* s =
      match p with
      | Cmd c -> exec_cmd s c
      | Seq (p1, p2) ->
          let* s = exec cfg s p1 in
          on_step s;
          exec cfg s p2
      | _ -> failwith "not implemented"
    in
    Ok (simplify s)

(* TODO check that simplification applies substitution *)
