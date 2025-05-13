open Base
open Executor_state

let ( let* ) x f = Option.bind x ~f
let ( let+ ) x f = Option.map x ~f
let simplify s = s

let add_path_cond s e =
  simplify Bexp.{ s with path_cond = Bop (And, s.path_cond, e) }

let assign s x e =
  let x' = Dummy.fresh_of_ide x in
  {
    s with
    (* x = x' ∧ x' = e *)
    dummies = Map.set s.dummies ~key:x ~data:x';
    path_cond = Bexp.(Bop (And, s.path_cond, Cmp (Eq, Var x', e)));
  }

let store s x' v = { s with heap = Map.set s.heap ~key:x' ~data:v }

(* TODO rivedere i let* per ISL *)
let exec_cmd s =
  let open Cmd in
  function
  | Skip -> Some s
  | Assert e -> Some (add_path_cond s e)
  | Assign (x, e) -> Some (assign s x e)
  | AssignFromRef (x, y) -> (
      (* if y has no dummy variable, it has never been used before
         (in the program or precondition) so it is not allocated *)
      let* y' = Map.find s.dummies y in
      let* e = Map.find s.heap y' in
      match e with Val e -> Some (assign s x e) | Undefined | Dealloc -> None)
  | AssignToRef (x, y) ->
      let* x' = Map.find s.dummies x in
      let v =
        match Map.find s.dummies y with
        | Some y' -> Aexp.Var y'
        (* uninitialized variables are 0 by default *)
        | None -> Aexp.Num 0
      in
      let+ _ = Map.find s.heap x' in
      store s x' (Val v)
  | Alloc x ->
      let x' = Dummy.fresh_of_ide x in
      Some
        (store
           { s with dummies = Map.set s.dummies ~key:x ~data:x' }
           x' Undefined)
  | Free x ->
      let* x' = Map.find s.dummies x in
      let+ _ = Map.find s.heap x' in
      store s x' Dealloc
  | Error -> None

let rec exec s p =
  let open Prog in
  let print_state s =
    PPrint.ToChannel.pretty 1. 60 Out_channel.stdout (Executor_state.pretty s)
  in
  print_state s;
  match p with
  | Cmd c -> exec_cmd s c
  | Seq (p1, p2) ->
      let* s = exec s p1 in
      print_state s;
      let+ s = exec s p2 in
      print_state s;
      s
  | _ -> failwith "not implemented"

(* TODO check that simplification applies substitution *)
