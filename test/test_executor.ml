open Base
open Stdio
open Lis_project

let pre = "x = y"

(* let prog = "x ← 1; y ← 2; x ← 3" *)
(* let prog = "free(x)" *)
(* let prog = "x ← alloc(); y ← 2; [x] ← y; z ← [x]; y ← 4; [x] ← y; free(x)" *)
(* let prog = "x ← alloc(); y ← x; free(y)" *)
let prog = "x ← (1 + 2)"

let parse_proposition (s : string) : Prop.t =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.eprop Lexer.read lexbuf in
  ast

let parse_program (s : string) : Prog.t =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.eprog Lexer.read lexbuf in
  ast

let print d =
  PPrint.ToChannel.pretty 1. 60 Out_channel.stdout d;
  Out_channel.print_endline "\n"

let test_executor () =
  let p = parse_program prog in

  let s =
    parse_proposition pre |> Simplify.simplify_prop |> Norm_prop.of_prop
    |> Simplify.simplify_t |> Executor_state.list_of_norm_prop |> List.hd_exn
  in
  print (Prog.pretty p);
  print (Executor_state.pretty s);
  let s = Executor.exec s p in
  match s with
  | Some s -> print (Executor_state.pretty s)
  | None -> Out_channel.print_endline "None"
