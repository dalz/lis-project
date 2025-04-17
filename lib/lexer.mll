(**
This file defines the common parser for SL+ and ISL+.
*)

(** Header *)
{
    open Parser
}

(** Identifiers - Named Regular Expressions *)
let white = [' ' '\t']+
let num = ['0'-'9']+
let ide = ['a'-'z' 'A'-'Z']+

(** rule *)
rule read =
    parse
    (** Common tokens *)
    | white { read lexbuf }
    | "(" { LPAREN }
    | ")" { RPAREN }
    (** Arithmetic tokens *)
    | num { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { MULT }
    | "/" { DIV }
    | "%" { MOD }
    | "-" { MINUS }
    (** Boolean tokens *)
    | "true" { BOOL (true) }
    | "false" { BOOL (false) }
    | "||" { OR }
    | "&&" { AND }
    | "!" { NOT }
    | "<" { LT }
    | "<=" { LE }
    | "=" { EQ }
    | eof {EOF}

