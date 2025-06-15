open Base
open Stdio
open Lis_project
open Lexing

let usage_msg = "lis_project [OPTIONS] [filename]"
let input_file = ref ""
let force_isl = ref false
let force_sl = ref false
let step_exec = ref false
let no_verbose = ref false
let anon_fun filename = input_file := filename

let speclist =
  [
    ("--isl", Stdlib.Arg.Set force_isl, "Force use of isl+");
    ("--sl", Stdlib.Arg.Set force_sl, "Force use of sl+");
    ( "--step-exec",
      Stdlib.Arg.Set step_exec,
      "Execute the derivation step by step" );
    ( "--no-verbose",
      Stdlib.Arg.Set no_verbose,
      "Skip the execution prints and shows the result" );
  ]

let print d =
  PPrint.ToChannel.pretty 1. 60 Out_channel.stdout d;
  Out_channel.print_endline "\n"

let print_state s = print (Executor_state.pretty s)

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Out_channel.fprintf outx "\n%d:%d" (pos.pos_lnum - 1)
    (pos.pos_cnum - pos.pos_bol + 1)

let _parse ch =
  let lexbuf = Lexing.from_channel ch in
  try Parser.input Lexer.read lexbuf
  with Parser.Error ->
    Out_channel.fprintf stderr "%a: Syntax error\n" print_position lexbuf;
    Stdlib.exit (-1)

let step_by_step s (c : Cmd.t) =
  let separator = "=========================" in
  if not !no_verbose then (
    Stdio.Out_channel.print_endline separator;
    Stdio.Out_channel.print_string "Current command : ";
    PPrint.ToChannel.pretty 1. 60 Out_channel.stdout (Cmd.pretty c);
    Stdio.Out_channel.print_endline "";
    Stdio.Out_channel.print_endline separator;

    print (Executor_state.pretty s);
    if !step_exec then
      let quit_loop = ref false in
      while not !quit_loop do
        Out_channel.print_string "n/c> ";
        Out_channel.flush Out_channel.stdout;
        let str = Stdlib.read_line () in
        if not (String.equal str "") then
          if Char.equal str.[0] 'n' then quit_loop := true
          else if Char.equal str.[0] 'c' then (
            quit_loop := true;
            step_exec := false)
          else Out_channel.print_endline "Invalid option!\n"
        else Out_channel.print_endline "Invalid option!\n"
      done)

(* Reads lines from stdin until eof or target successive empty lines *)
let rec get_lines acc count target =
  try
    match (Stdlib.read_line (), count) with
    | "", n ->
        if Int.equal n (target - 1) then acc
        else get_lines (acc ^ "\n") (n + 1) target
    | x, _ -> get_lines (acc ^ x ^ "\n") 0 target
  with End_of_file -> acc

let () =
  Stdlib.Arg.parse speclist anon_fun usage_msg;
  let fname = !input_file in
  let exec =
    if !force_isl then Isl_executor.exec ~interactive:!step_exec
    else if !force_sl then Sl_executor.exec
    else if String.equal (Stdlib.Filename.extension fname) ".isl" then
      Isl_executor.exec ~interactive:!step_exec
    else if String.equal (Stdlib.Filename.extension fname) ".sl" then
      Sl_executor.exec
    else if not (String.equal fname "") then
      failwith "Extension of input file should be .isl or .sl"
    else failwith "To read program from stdin you must specify --sl or --isl"
  in

  let pre, prog =
    if
      (* If fname is empty read program from stdin *)
      String.equal fname ""
    then
      let pr = get_lines "" 0 2 in
      let lexbuf = Lexing.from_string pr in
      Parser.input Lexer.read lexbuf
    else
      In_channel.with_file fname ~f:(fun ch ->
          let lexbuf = Lexing.from_channel ch in
          Parser.input Lexer.read lexbuf)
  in

  (* Prop.show pre |> Out_channel.print_endline;*)
  (* Prog.show prog |> Out_channel.print_endline;*)
  let prog_vars =
    let ps, ds = Prog.fv prog in
    List.map ps ~f:Dummy.raw_of_ide @ ds
  in
  let pre = Norm_prop.of_prop ~prog_vars pre |> Norm_prop.simpl in
  Out_channel.print_endline "Simplified precondition:\n";
  print (Norm_prop.pretty pre);
  Executor_state.list_of_norm_prop pre
  |> List.concat_map ~f:(fun s ->
         Out_channel.print_endline
           "\n=========================\nExecution from state:\n";
         print (Executor_state.pretty s);
         exec ~on_step:step_by_step s prog)
  |> function
  | [] -> Out_channel.print_endline "⊥ (all branches pruned)"
  | ps ->
      List.iter ps ~f:(function
        | Executor.Ok s -> print (Prop.pretty s)
        | Err s ->
            Out_channel.print_endline "[error]";
            print_state s
        | Stuck s ->
            Out_channel.print_endline "[stuck]";
            print_state s)
