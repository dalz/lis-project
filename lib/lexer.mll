(**
This file defines the common parser for SL+ and ISL+.
*)

(** Header *)
{
    open Parser
}

(** Identifiers - Named Regular Expressions *)
let white = [' ' '\t' '\n']+
let num = ['0'-'9']+
let ide = ['a'-'z' 'A'-'Z']+
let comment = '#' [^ '\n' '\r']*
let dummy_ide = ide '\''+

(** rule *)
rule read =
    parse
    (** Common tokens *)
    | white { read lexbuf }
    | "(" { LPAREN }
    | ")" { RPAREN }

    (** Triples *)
    | "{" { LBRACE }
    | "}" { RBRACE }

    (** Arithmetic tokens *)
    | num { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | "+" { PLUS }
    | "-" { MINUS }
    | "**" | "×" { MULT }
    | "/" { DIV }
    | "%" { MOD }
    | "-" { MINUS }

    (** Boolean tokens *)
    | "true" | "⊤" { BOOL (true) }
    | "false" | "⊥" { BOOL (false) }
    | "||" | "∨" { OR }
    | "&&" | "∧" { AND }
    | "!" | "¬" { NOT }
    | "<" { LT }
    | "<=" | "≤" { LE }
    | "=" { EQ }
    
    (** Atom tokens *)
    | "emp" {EMP}
    | "->" | "↦" {REF}
    | "!->" | "!↦" | "↦̸" {NREF}
    
    (** Proposition tokens*)
    | "exists" | "∃" {EXIST}
    | "*" | "∗" {SEP}
    | "." {DOT}
    
    (** Program tokens *)
    | ";" { SEMICOLON }
    
    (** Command tokens *)
    | "skip" { SKIP }
    | "?" { QUESTION }
    | "[" { LBRACK }
    | "]" { RBRACK }
    | ":=" | "←" { ASSIGN }
    | "alloc" { ALLOC }
    | "free" { FREE }
    | "error" { ERROR }
    | "star" | "⋆" {STAR}
    (** string token *)
    | ide { ID (Lexing.lexeme lexbuf) }
    | dummy_ide as s {
        let len = String.length s in
        let i = ref (len - 1) in
        while !i >= 0 && s.[!i] = '\'' do
        decr i
        done;
        let letters_part = String.sub s 0 (!i + 1) in
        let apostrophes_part = String.sub s (!i + 1) (len - !i - 1) in
        let apostrophes_len = String.length apostrophes_part in
        DUMMY_ID (letters_part, apostrophes_len)
    }
    | eof {EOF}
    | comment { Lexing.new_line lexbuf; read lexbuf }
    | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }
