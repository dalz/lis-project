open Base
open Stdio
open Lis_project

(* let parse_input (s : string) : Prop.t = *)
(*   let lexbuf = Lexing.from_string s in *)
(*   Parser.input Lexer.read lexbuf *)

let print d =
  PPrint.ToChannel.pretty 1. 60 Out_channel.stdout d;
  Out_channel.print_endline "\n"

let () =
  let fname = (Sys.get_argv ()).(1) in
  let pre, prog =
    In_channel.with_file fname ~f:(fun ch ->
        let lexbuf = Lexing.from_channel ch in
        Parser.input Lexer.read lexbuf)
  in
  let prog_vars =
    let ps, ds = Prog.fv prog in
    List.map ps ~f:Dummy.raw_of_ide @ ds
  in
  let pre =
    Simplify.simplify_prop pre
    |> Norm_prop.of_prop ~prog_vars
    |> Simplify.simplify_t
  in
  Out_channel.print_endline "Simplified precondition:\n";
  print (Norm_prop.pretty pre);
  Executor_state.list_of_norm_prop pre
  |> List.iter ~f:(fun s ->
         Out_channel.print_endline
           "\n=========================\nExecution from state:\n";
         print (Executor_state.pretty s);
         match Executor.exec s prog with
         | Some s -> print (Executor_state.pretty s)
         | None -> Out_channel.print_endline "None")

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
