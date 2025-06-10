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
      s with
      heap = Map.set s.heap ~key:x' ~data:Undefined;
      dummies = Map.set s.dummies ~key:x ~data:x';
    }

(** Auxiliary function of choice_rule. It Lets the user decide which path to
    take *)
let rec interactive_choice_rule s p1 p2 =
  (* Printing message to the user *)
  Stdio.Out_channel.print_endline "Non-det choice between:\n\t- left_path:";
  PPrint.ToChannel.pretty 1. 60 Out_channel.stdout (Prog.pretty p1);
  Stdio.Out_channel.print_endline "\n\t- right_path:";
  PPrint.ToChannel.pretty 1. 60 Out_channel.stdout (Prog.pretty p2);
  Stdio.Out_channel.print_endline
    "Input 'LF' for selecting the left path, 'RT' for the right path, 'BT' for \
     doing both and 'RN' for choosing at random (default is 'RN').";

  (* Getting user's choice from stdin *)
  let user_choice =
    Option.value (In_channel.input_line In_channel.stdin) ~default:"R"
  in

  (* Going through the selected path *)
  match user_choice with
  | "LF" -> 0
  | "RT" -> 1
  | "BT" -> 3
  | "RN" -> Random.int 3
  | _ ->
      Stdio.Out_channel.output_string Stdio.stdout "Invalid input, retrying...";
      interactive_choice_rule s p1 p2

(** Executes the choice rule. If `interactive` is enabled, then the user decides
    which path to take, otherwise the function selects at random. *)
let choice_rule ?(interactive = true) (s : Executor_state.t) p1 p2 =
  let choice =
    if interactive then interactive_choice_rule s p1 p2 else Random.int 3
  in
  match choice with
  | 0 -> [ (s, p1) ]
  | 1 -> [ (s, p2) ]
  | 2 -> [ (s, p1); (s, p2) ]
  | _ -> failwith "In choice rule: choice >= 3"

let iter_rule exec ( let* ) s p =
  let rec unroll s n =
    if n = 0 then [ Ok s ]
    else
      let* s = exec s p in
      unroll s (n - 1)
  in
  unroll s 7

let exec ~on_step s p =
  Executor.exec { bind; on_step; alloc_rule; choice_rule; iter_rule } s p
  |> List.map ~f:(function
       | Ok s -> Ok (Executor_state.to_prop s)
       | Err s -> Err s
       | Stuck s -> Stuck s)
