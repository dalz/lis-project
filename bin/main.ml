open Base
open Stdio
open Lis_project
open Lexing

(* TODO add ≠ to parser *)
let usage_msg = "lis_project [OPTIONS] [filename]";;
let input_file = ref "";;
let force_isl = ref false;;
let force_sl = ref false;;
let step_exec = ref false;;
let no_verbose = ref false;;

let anon_fun filename =
        input_file := filename;;
let speclist =
        [("--isl", Stdlib.Arg.Set force_isl, "Uses isl");
         ("--sl", Stdlib.Arg.Set force_sl, "Uses sl");
         ("--step-exec", Stdlib.Arg.Set step_exec, "Execute the derivation step by step");
         ("--no-verbose", Stdlib.Arg.Set no_verbose, "Skip the execution prints and shows the result")
        ];;

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
    Out_channel.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    Stdlib.exit (-1)

let step_by_step s = 
  if not !no_verbose then (
    print (Executor_state.pretty s);
    if !step_exec then (
      let quit_loop = ref false in
      while not !quit_loop do
        let str = Stdlib.read_line () in
        if not (String.equal str "") then (
          if Char.equal str.[0] 'n' then quit_loop := true else
          if Char.equal str.[0] 'c' then (quit_loop := true; step_exec := false)
          else Out_channel.print_endline "Invalid option!\n";)
          else Out_channel.print_endline "Invalid option!\n";
      done
    )
  )

(* Reads lines from stdin until eof or target successive empty lines *)
let rec get_lines acc count target = 
        try match Stdlib.read_line(), count with
            | "", n -> if Int.equal n (target-1) then acc 
                else get_lines (acc^"\n") (n+1) target
            | x,_ -> get_lines (acc^x^"\n") 0 target
        with End_of_file -> acc;;


let () =
  Stdlib.Arg.parse speclist anon_fun usage_msg;
  let fname = !input_file in
  let exec = if !force_isl then Isl_executor.exec else 
    if !force_sl then Sl_executor.exec else
      if String.equal (Stdlib.Filename.extension fname) ".isl" then Isl_executor.exec else 
        if String.equal (Stdlib.Filename.extension fname) ".sl" then Sl_executor.exec else
          failwith "Extension of input file should be .isl or .sl" in

  let pre, prog =(
    (* If fname is empty read program from stdin *)
    if String.equal fname "" then
        let pr = get_lines "" 0 2 in
        pr |> Out_channel.print_endline;
        let lexbuf = Lexing.from_string pr in
        Parser.input Lexer.read lexbuf
    else
    In_channel.with_file fname ~f:(fun ch ->
        let lexbuf = Lexing.from_channel ch in
        Parser.input Lexer.read lexbuf)
  ) in

  Prop.show pre |> Out_channel.print_endline;
  Prog.show prog |> Out_channel.print_endline;
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
(* let () = Out_channel.print_endline (Aexp.show (Aexp.simpl (Num 1))) *)

(*
let () =
  let propositions_to_test =
    [ "x = y && y = 5" ]
    (* TODO not working *)
    (*
  Both true and false return an empty list, is this ok???
  "exists x. true" -> [] (works)
  "exists x. false" -> [] (works)
  "exists x. true || false" -> [] (works)
  "exists x. true && false" -> NOT SURE, STRANGE OUTPUT, DOUBLE CHECK
  "exists x. true * true" -> [] (works)
  "exists x. true * false" -> [false] conj [] shouldn't the second empty list be removed? 
  "exists x. y -> 3 + 2" -> fails, correct?? 
  "exists x. (true * x -> 5)" -> fails, correct??
  "exists x. emp" -> fails, correct??
  "exists x. true * emp" -> [] works
  "exists x. emp * true" -> fails, correct??
  "x-> 5 * y -> 3" -> no changes (works)
  "x-> 5 * y -> 3 + 2" -> x -> 5 * y-> 5 (works)
  "false * true" -> false (works)
  "false * true" -> false (works but leaves the empty list behind like the case above, shouldn't that be removed?)
  "x -> 5 * y -> 10" -> same (works)
  "x -> 5 * x -> 5" -> x -> 5 (works) 
  "x -> 5 * x -> 5 * x -> 5" -> x -> 5 (works)
  "emp * x -> 5" -> x -> 5 (works)
  "x -> 5 * emp" -> x -> 5 (works)
  "emp * emp * emp" -> emp (works)
  "x -> 5 * emp * emp * emp" -> x -> 5 (works)
  *)
  in
  List.iter
    ~f:(fun x ->
      let parsed_prop =
        parse_proposition x |> Lis_project.Simplify.simplify_prop
      in
      let normalized_ast = Lis_project.Norm_prop.of_prop parsed_prop in
      let simplified = Lis_project.Simplify.simplify_t normalized_ast in
      print_string "\nProp after second simplification\n";
      print_string (Lis_project.Norm_prop.show simplified);
      print_newline ())
    propositions_to_test
 *)
