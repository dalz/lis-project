(**
This file defines the common parser for SL+ and ISL+.
*)

(** Header *)
%{
    open Ast
%}

(** Exp Token *)
%token EOF
%token LPAREN
%token RPAREN

(** Aexp Tokens *)
%token <int> INT
%token PLUS
%token MINUS
%token MULT
%token DIV
%token MOD

(* Bexp Tokens *)
%token <bool> BOOL
%token AND
%token OR
%token NOT
%token LT
%token LE 
%token EQ

(** Precedence and associativity *)
%left PLUS
%left MINUS
%left MULT
%left DIV
%left MOD

%left OR
%left AND
%nonassoc NOT
%nonassoc EQ
%nonassoc LT 
%nonassoc LE
%nonassoc LPAREN RPAREN


(** Declaring types *)
%type <Ast.aexp> aexp
%type <Ast.bexp> bexp

(** Declaring the starting point *)
%start <Ast.prog> prog

%% (** ends the declaration section *)

(** Grammar rules *)

prog :
    | e = exp;EOF { e }
    ;

exp :
    | LPAREN; e=exp; RPAREN {e}
    | b = bexp { Cmd (Assert b) }
    ;

aexp :
    | LPAREN; a=aexp; RPAREN {a}
    | i = INT  { Num i }
    | a1 = aexp PLUS a2 = aexp { Bop(Sum, a1, a2) } 
    | a1 = aexp MINUS a2 = aexp { Bop(Sub, a1, a2) }
    | a1 = aexp MULT a2 = aexp { Bop(Mul, a1, a2) }
    | a1 = aexp DIV a2 = aexp { Bop(Div, a1, a2) }
    | a1 = aexp MOD a2 = aexp { Bop(Mod, a1, a2) }
    | MINUS a = aexp { Uop(Neg, a) }
    ;

bexp :
    | LPAREN; b=bexp; RPAREN {b}
    | b = BOOL { Bool b }
    | a1 = aexp LT a2 = aexp { Cmp(Lt, a1, a2) }
    | a1 = aexp LE a2 = aexp { Cmp(Le, a1, a2) }
    | a1 = aexp EQ a2 = aexp { Cmp(Eq, a1, a2) }
    | b1 = bexp AND b2 = bexp { BAnd(b1, b2) }
    | b1 = bexp OR b2 = bexp { BOr(b1, b2) }
    | NOT b = bexp { Not b }
    ;






