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
%token MULT
%token DIV
%token MOD

(* Bexp Tokens *)
%token <bool> BOOL
%token BOOL_AND
%token SEP
%token BOOL_OR
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
%token PROP_OR
%token PROP_AND

(** Program Tokens *)
%token SEMICOLON

(** Command Tokens *)
%token SKIP
%token QUESTION
%token LBRACK
%token RBRACK
%token STAR
%token ASSIGN
%token ALLOC
%token FREE
%token ERROR

(** String Token *)
%token <string> ID

(** Precedence and associativity *)
%left PLUS MINUS
%left MULT DIV MOD
%left BOOL_OR
%left BOOL_AND
%left SEMICOLON 
%left PROP_OR
%left SEP
%left PROP_AND
%right ASSIGN  
%nonassoc NOT 
%nonassoc DOT

(* Dummy tokens *)
%left PREFER_A
%nonassoc RPAREN

(** Declaring types *)
%type <Aexp.t>   aexp
%type <Bexp.t>   bexp
%type <Atom.t>   atom
%type <Ast.prop> prop
%type <Ast.prog> prog 
%type <Ast.prop> eprop
%type <Ast.prog> eprog 
(** Declaring the starting point *)
%start eprop eprog

%% (** ends the declaration section *)

(** Grammar rules *)

eprog :
    | p = prog; EOF { p }
    ;

eprop :
    | p = prop; EOF { p }
    ;

(* Program Rule *)

prog :
    | LPAREN; p=prog; RPAREN {p}
    | p1 = prog;  SEMICOLON; p2 = prog {Seq (p1, p2)}
    | p1 = prog; PLUS; p2 = prog {Choice (p1, p2)}
    | STAR; LPAREN; p = prog; RPAREN {Star(p)}
    | c = cmd {Cmd c}
    ;

(* Command Rule *)
cmd :
    | SKIP {Skip}
    | b = bexp; QUESTION { Assert b }
    | s = ID; ASSIGN; a = aexp {
        let t = raw_of_string s in
        Assign (t, a)
    }
    | s1 = ID; ASSIGN; LBRACK s2 = ID RBRACK {
        let t1 = raw_of_string s1 in
        let t2 = raw_of_string s2 in
        AssignFromRef (t1, t2)
    }
    | LBRACK; s1 = ID; RBRACK; ASSIGN; s2 = ID {
        let t1 = raw_of_string s1 in
        let t2 = raw_of_string s2 in
        AssignToRef (t1, t2)
    }
    | s = ID; ASSIGN; ALLOC; LPAREN; RPAREN {
        let t = raw_of_string s in
        Alloc t
    }
    | FREE; LPAREN; s = ID; RPAREN {
        let t = raw_of_string s in
        Free t
    }
    | ERROR; LPAREN; RPAREN {
        Error
    }
    ;

(* Proposition Rule *)
prop :
    | LPAREN; p=prop; RPAREN {p}
    | a = atom {Atom a}
    | p1 = prop; PROP_AND; p2 = prop {And (p1, p2)}
    | p1 = prop; PROP_OR; p2 = prop {Or (p1, p2)}
    | p1 = prop; SEP ; p2 = prop {Sep (p1, p2)} 
    | EXIST; s = ID; DOT p = prop {
        let t = raw_of_string s in
        Exists (t, p) 
    }
    ;
 

(* Atom Rule *)
atom :
    | b = bexp %prec PREFER_A { Bool b }
    | s = ID; REF; a = aexp { 
        let t = raw_of_string s in 
        PointsTo (t, a)
     }
    | s = ID; NREF {
        let t = raw_of_string s in
        PointsToNothing t
    }
    | EMP { Emp }
    ;

(* Arithmetic Rule *)
aexp :
    | LPAREN; a=aexp; RPAREN {a}
    | i = INT  { Num i }
    | a1 = aexp; PLUS; a2 = aexp { Bop(Sum, a1, a2) } 
    | a1 = aexp; MINUS; a2 = aexp { Bop(Sub, a1, a2) }
    | a1 = aexp; MULT; a2 = aexp { Bop(Mul, a1, a2) }
    | a1 = aexp; DIV; a2 = aexp { Bop(Div, a1, a2) }
    | a1 = aexp; MOD; a2 = aexp { Bop(Mod, a1, a2) }
    | MINUS; a = aexp { Uop(Neg, a) }
    | s = ID { 
        let t = raw_of_string s in
        Var t
     }
    ;

(* Boolean Rule *)
bexp :
    | LPAREN; b=bexp; RPAREN {b}
    | b = BOOL { Const b }
    | a1 = aexp LT a2 = aexp { Cmp(Lt, a1, a2) }
    | a1 = aexp LE a2 = aexp { Cmp(Le, a1, a2) }
    | a1 = aexp EQ a2 = aexp { Cmp(Eq, a1, a2) }
    | b1 = bexp BOOL_AND b2 = bexp { Bop(And, b1, b2) }
    | b1 = bexp BOOL_OR b2 = bexp { Bop(Or, b1, b2) }
    | NOT b = bexp { Not b }
    ;