open Base
open Executor

let bind x f =
  match x with
  | Ok s -> f s
  | Err s ->
      List.map (f s) ~f:(function Ok z -> Err z | (Err _ | Stuck _) as z -> z)
  | Stuck _ -> [ x ]

(** Handles how to allocate the requested memory in the heap. By default it
    always try to reuse existing deallocated locations (Alloc2 rule), but the
    user can decide which part of memory deallocate or instead allocating a
    fresh new one without touching the stack. *)
let alloc_rule ~interactive s x =
  let open Executor_state in
  let user_message =
    Printf.sprintf
      "Select one among the following strategies for allocating the new \
       variable %s :"
      (Ide.to_string x)
  in
  let get_user_choice () =
    Stdio.Out_channel.print_endline user_message;
    Stdio.Out_channel.print_endline
      "1 - Reuse the first deallocated location the program can find (if any \
       exists)";
    Stdio.Out_channel.print_endline
      "2 - Choose explicitly which among the deallocated entries to reuse";
    Stdio.Out_channel.print_endline "3 - Allocate a new entry entirely";
    Stdio.Out_channel.print_endline "Input (default behavior is strategy '1')";

    let input =
      In_channel.input_line In_channel.stdin
      |> Option.value ~default:"1" |> Int.of_string_opt
      |> Option.value ~default:1
    in
    match input with
    | n when n >= 1 && n <= 3 ->
        let str = Printf.sprintf "Chosen stategy %d" n in
        Stdio.Out_channel.print_endline str;
        n
    | _ -> failwith "Invalid input: must be between 1 and 3"
  in

  let find_first_deallc () =
    Map.to_alist s.heap
    |> List.find_map ~f:(function x', Dealloc -> Some x' | _ -> None)
    |> Option.value_or_thunk ~default:(fun () -> Dummy.fresh_of_ide x)
  in

  let find_specific_deallc () =
    let available_list =
      Map.to_alist s.heap
      |> List.filter_map ~f:(function x', Dealloc -> Some x' | _ -> None)
    in

    let available_string =
      available_list
      |> List.mapi ~f:(fun i x ->
             Printf.sprintf "%d) %s ; " (i + 1) (Dummy.to_string x))
      |> String.concat
    in

    let select_location () =
      Stdio.Out_channel.print_endline
        "Select the index of one among the available locations to deallocate:";
      Stdio.Out_channel.print_endline available_string;
      let choice =
        Stdio.Out_channel.print_endline "Input (default value is '1'):";
        In_channel.input_line In_channel.stdin
        |> Option.value ~default:"1" |> Int.of_string_opt
        |> Option.value ~default:1
      in
      List.nth available_list (choice - 1)
      |> Option.value_or_thunk ~default:(fun () -> Dummy.fresh_of_ide x)
    in

    if List.length available_list > 0 then select_location ()
    else (
      Stdio.Out_channel.print_endline
        "No memory location to deallocate, allocating a new entry";
      Dummy.fresh_of_ide x)
  in

  let new_alloc () = Dummy.fresh_of_ide x in

  let x' =
    match if interactive then get_user_choice () else 1 with
    | 1 -> find_first_deallc ()
    | 2 -> find_specific_deallc ()
    | 3 -> new_alloc ()
    | _ -> failwith "strategy >= 3"
  in
  Ok
    {
      s with
      heap = Map.set s.heap ~key:x' ~data:Undefined;
      dummies = Map.set s.dummies ~key:x ~data:x';
    }

(** Executes the choice rule. If `interactive` is enabled, then the user decides
    which path to take, otherwise the function selects at random. *)
let choice_rule ~interactive (s : Executor_state.t) p1 p2 =
  (* Handles user's input*)
  let rec get_input_choice_rule p1 p2 =
    (* Printing message to the user *)
    Stdio.Out_channel.print_string "Non-det choice between:\n- left_path:";
    PPrint.ToChannel.pretty 1. 60 Out_channel.stdout (Prog.pretty p1);
    Stdio.Out_channel.print_string "\n- right_path:";
    PPrint.ToChannel.pretty 1. 60 Out_channel.stdout (Prog.pretty p2);
    Stdio.Out_channel.print_endline
      "\n\
       1 - Selects the left path\n\
       2 - Selects the right path\n\
       3 - Selects both\n\
       4 - Let the program choose at random";

    (* Getting user's choice from stdin *)
    let user_choice =
      Stdio.Out_channel.print_endline "Input (default is '4'):";
      In_channel.input_line In_channel.stdin
      |> Option.value ~default:"4" |> Int.of_string_opt
      |> Option.value ~default:4
    in

    (* Going through the selected path *)
    match user_choice with
    | 1 | 2 | 3 -> user_choice - 1
    | 4 -> Random.int 3
    | _ ->
        Stdio.Out_channel.print_endline "\nInvalid input, retrying...";
        get_input_choice_rule p1 p2
  in

  let choice =
    if interactive then get_input_choice_rule p1 p2 else Random.int 3
  in
  match choice with
  | 0 -> [ (s, p1) ]
  | 1 -> [ (s, p2) ]
  | 2 -> [ (s, p1); (s, p2) ]
  | _ -> failwith "In choice rule: choice >= 3"

(** Handles loops. In practice it runs the cycle only for a certain number of
    steps chosen by the user (the default value is 7). The user also decides if
    it wants to consider all the states produced inside the loop or only the
    ones after n interations have been done. *)
let iter_rule ~interactive exec ( let* ) s p =
  let rec get_input_iter_rule p =
    (* Getting the number of iterations *)
    Stdio.Out_channel.print_string "Given `p`:";
    PPrint.ToChannel.pretty 1. 60 Out_channel.stdout (Prog.pretty p);
    Stdio.Out_channel.print_endline
      "\n\
       Choose how many steps of `p` you want to unroll\n\
       Input (default is '7'): ";
    let num_iter =
      In_channel.input_line In_channel.stdin
      |> Option.value ~default:"7" |> Int.of_string_opt
      |> Option.value ~default:7
    in

    (* Getting the preference of unroll *)
    Stdio.Out_channel.print_endline
      "Choose which states you want to consider:\n\
       1 - Return only the states after the `n` iterations have been executed\n\
       2 - Return all the states computed in-between\n\
       Input (default is '1'):";
    let user_choice =
      In_channel.input_line In_channel.stdin
      |> Option.value ~default:"1" |> Int.of_string_opt
      |> Option.value ~default:1
    in
    match user_choice with
    | 1 -> (num_iter, false)
    | 2 -> (num_iter, true)
    | _ ->
        Stdio.Out_channel.print_endline "Invalid input, retrying...";
        get_input_iter_rule p
  in

  (* Let the user choose the number of interations and what it wants in return*)
  let num_iter, is_all =
    if interactive then get_input_iter_rule p else (7, false)
  in

  let rec unroll s n acc : 'a status list =
    if n = 0 then acc
    else
      let l = exec s p in
      let* s = l in
      let acc = if is_all then acc @ l else [ Ok s ] in
      unroll s (n - 1) acc
  in
  if num_iter = 0 then [ Ok s ] else unroll s num_iter []

let exec ?(denoise = true) ~on_step ~interactive s p =
  Executor.exec
    {
      bind;
      on_step;
      alloc_rule = alloc_rule ~interactive;
      choice_rule = choice_rule ~interactive;
      iter_rule = iter_rule ~interactive;
    }
    s p
  |> List.map ~f:(function
       | Ok s ->
           let post =
             if denoise then Executor_state.dummy_dismantler s else s
           in
           Ok (Executor_state.to_prop post)
       | Err s -> Err s
       | Stuck s -> Stuck s)
