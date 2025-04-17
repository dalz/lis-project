(**
This file defines the common parser for SL+ and ISL+.
*)

(** Header *)
%{
    open Ast
    open Ide
%}

(** Exp Token *)
%token EOF
%token LPAREN
%token RPAREN
(** Aexp Tokens *)
%token <int> INT
%token PLUS
%token MINUS
%token STAR
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
(** Atom Tokens *)
%token EMP
%token REF
%token NREF
(** Proposition Tokens *)
%token EXIST
%token DOT
(** String Token *)
%token <string> ID



(** Precedence and associativity *)
%left PLUS MINUS STAR DIV MOD
%left OR AND


(** Declaring types *)
%type <Ast.aexp> aexp
%type <Ast.bexp> bexp
%type <Ast.atom> atom
%type <Ast.prop> prop
(** Declaring the starting point *)
%start <Ast.prop> prog

%% (** ends the declaration section *)

(** Grammar rules *)

prog :
    | e = exp;EOF { e }
    ;

exp :
    | LPAREN; e=exp; RPAREN {e}
    | p = prop { p }
    ;

(* Atom Rule *)
atom :
    | LPAREN; at=atom; RPAREN {at}
    | b = bexp { Bool b }
    | s = ID REF a = aexp { 
        let t = raw_of_string s in 
        PointsTo (t, a)
     }
    | s = ID NREF {
        let t = raw_of_string s in
        PointsToNothing t
    }
    | EMP { Emp }
    ;


(* Proposition Rule *)
prop :
    | LPAREN; p=prop; RPAREN {p}
    | a = atom {Atom a}
    | p1 = prop AND p2 = prop {And (p1, p2)}
    | p1 = prop OR p2 = prop {Or (p1, p2)}
    | p1 = prop STAR p2 = prop {Sep (p1, p2)} 
    | EXIST s = ID DOT p = prop {
        let t = raw_of_string s in
        Exists (t, p) 
    }
    ;

(* Arithmetic Rule *)
aexp :
    | LPAREN; a=aexp; RPAREN {a}
    | i = INT  { Num i }
    | a1 = aexp PLUS a2 = aexp { Bop(Sum, a1, a2) } 
    | a1 = aexp MINUS a2 = aexp { Bop(Sub, a1, a2) }
    | a1 = aexp STAR a2 = aexp { Bop(Mul, a1, a2) }
    | a1 = aexp DIV a2 = aexp { Bop(Div, a1, a2) }
    | a1 = aexp MOD a2 = aexp { Bop(Mod, a1, a2) }
    | MINUS a = aexp { Uop(Neg, a) }
    | s = ID { 
        let t = raw_of_string s in
        Var t
     }
    ;

(* Boolean Rule *)
bexp :
    | LPAREN; b=bexp; RPAREN {b}
    | b = BOOL { BConst b }
    | a1 = aexp LT a2 = aexp { Cmp(Lt, a1, a2) }
    | a1 = aexp LE a2 = aexp { Cmp(Le, a1, a2) }
    | a1 = aexp EQ a2 = aexp { Cmp(Eq, a1, a2) }
    | b1 = bexp AND b2 = bexp { BAnd(b1, b2) }
    | b1 = bexp OR b2 = bexp { BOr(b1, b2) }
    | NOT b = bexp { Not b }
    ;


