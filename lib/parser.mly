(**
This file defines the common parser for SL+ and ISL+.
*)

(** Header *)
%{
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
%token SEP
%token OR
%token NOT
%token LT
%token LE 
%token EQ
%token NEQ

(** Atom Tokens *)
%token EMP
%token REF
%token NREF
%token SOMETHING

(** Proposition Tokens *)
%token EXIST
%token DOT

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
%token <string * int> DUMMY_ID

%token LBRACE
%token RBRACE

(** Precedence and associativity *)
%left OR AND SEP
%right ASSIGN  
%nonassoc NOT 
%nonassoc DOT
%nonassoc SEMICOLON
%left PLUS MINUS
%left MULT DIV MOD

(* Dummy tokens *)
%nonassoc PREFER_A
%nonassoc RPAREN

(** Declaring types *)
%type <Aexp.t> aexp
%type <Bexp.t> bexp
%type <Atom.t> atom
%type <Prop.t> prop
%type <Prog.t> prog
%type <Prop.t * Prog.t> input
%type <Dummy.t> id
(** Declaring the starting point *)
%start input

%% (** ends the declaration section *)

input :
    | LBRACE; pre = prop; RBRACE; prog = prog; EOF { 
        (* Printf.printf "Parser parsed a correct input\n" ;*)
        (pre, prog) }
    ;

(* Program Rule *)

prog :
    | p = prog_nt; SEMICOLON?; EOF { p }

prog_nt :
    | p1 = prog_nt; SEMICOLON; p2 = prog_nt { Prog.Seq (p1, p2) }
    | LPAREN; p=prog_nt; RPAREN {p}
    | p1 = prog_nt; PLUS; p2 = prog_nt {Prog.Choice (p1, p2)}
    | LPAREN; p = prog_nt; RPAREN; STAR {Prog.Iter(p)}
    | c = cmd { Prog.Cmd c }
    ;

/* prog_seq: */
/*     | c = cmd; SEMICOLON? { Prog.Cmd c } */
/*     | c = cmd; SEMICOLON; p = prog_seq { Prog.Seq (Prog.Cmd c, p) } */
/*     ; */

(* Command Rule *)
cmd :
    | SKIP {Skip}
    | b = bexp; QUESTION { Cmd.Assert b }
    | s = ID; ASSIGN; a = aexp {
        let t = Ide.raw_of_string s in
        Cmd.Assign (t, a)
    }
    | s1 = ID; ASSIGN; LBRACK s2 = ID RBRACK {
        let t1 = Ide.raw_of_string s1 in
        let t2 = Ide.raw_of_string s2 in
        Cmd.AssignFromRef (t1, t2)
    }
    | LBRACK; s1 = ID; RBRACK; ASSIGN; s2 = ID {
        (* Printf.printf "Parser Found a AssignToRef\n" ;*)
        let t1 = Ide.raw_of_string s1 in
        let t2 = Ide.raw_of_string s2 in
        Cmd.AssignToRef (t1, t2)
    }
    | s = ID; ASSIGN; ALLOC; LPAREN; RPAREN {
        let t = Ide.raw_of_string s in
        Cmd.Alloc t
    }
    | FREE; LPAREN; s = ID; RPAREN {
        let t = Ide.raw_of_string s in
        Cmd.Free t
    }
    | ERROR; LPAREN; RPAREN {
        Cmd.Error
    }
    ;

(* Proposition Rule *)
prop :
    | LPAREN; p=prop; RPAREN {p}
    | a = atom {Atom a}
    | p1 = prop; AND; p2 = prop {And (p1, p2)}
    | p1 = prop; OR; p2 = prop {Or (p1, p2)}
    | p1 = prop; SEP ; p2 = prop {Sep (p1, p2)}
    | EXIST; s = id; DOT p = prop {Exists (s, p)}
    ;

(* Id Rule *)
id :
    | s = ID {Dummy.raw s 0}
    | s = DUMMY_ID {
        let id = fst s in
        let num = snd s in
        Dummy.raw id num
    }

(* Atom Rule *)
atom :
    | b = bexp %prec PREFER_A { Bool b }
    | s = id; REF; SOMETHING {
        (* Printf.printf "Parser Found a PointToUndefined\n" ;*)
        PointsToUndefined s}
    | s = id; REF; a = aexp { 
        (* Printf.printf "Parser Found a PointTo\n" ;*)
        PointsTo (s, a)}
    | s = id; NREF { PointsToNothing s}
    | EMP { Emp }
    ;

(* Arithmetic Rule *)
aexp :
    | LPAREN; a=aexp; RPAREN {a}
    | i = INT  { Num i }
    | a1 = aexp;  PLUS; a2 = aexp { Bop(Sum, a1, a2) } 
    | a1 = aexp; MINUS; a2 = aexp { Bop(Sub, a1, a2) }
    | a1 = aexp; MULT; a2 = aexp { Bop(Mul, a1, a2) }
    | a1 = aexp; DIV; a2 = aexp { Bop(Div, a1, a2) }
    | a1 = aexp; MOD; a2 = aexp { Bop(Mod, a1, a2) }
    | MINUS; a = aexp { Uop(Neg, a) }
    | s = id { Var s}
    ;

(* Boolean Rule *)
bexp :
    | LPAREN; b=bexp; RPAREN {b}
    | b = BOOL { Const b }
    | a1 = aexp LT a2 = aexp { Cmp(Lt, a1, a2) }
    | a1 = aexp LE a2 = aexp { Cmp(Le, a1, a2) }
    | a1 = aexp EQ a2 = aexp { Cmp(Eq, a1, a2) }
    | a1 = aexp NEQ a2 = aexp { Cmp(Ne, a1, a2) }
    | b1 = bexp AND b2 = bexp { Bop(And, b1, b2) }
    | b1 = bexp OR b2 = bexp { Bop(Or, b1, b2) }
    | NOT b = bexp { Not b }
    ;
