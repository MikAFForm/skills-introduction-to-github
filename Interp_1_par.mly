%{
open Utils

let rec mk_app e es =
  match es with
  |[] -> e
  |x :: es -> mk_app (App(e, x)) es
%}

(*Make stuff to handle errors at some point*)
%token <int> NUM
%token <string> VAR
%token EOF
%token LET
%token FUN  (*Expressions*)
%token ARROW

%token IN

%token IF 
%token THEN 
%token ELSE
%token TRUE
%token FALSE
%token UNIT

%token LPAREN
%token RPAREN

%token ADD (*Operators*)
%token SUB
%token MUL
%token DIV
%token MOD
%token LT (*Comparisons*)
%token GT 
%token LTE 
%token GTE 
%token EQ 
%token NEQ

%token AND
%token OR

%right OR 
%right AND 
%left LT LTE GT GTE EQ NEQ 
%left ADD SUB
%left MUL DIV MOD
 (* All Comparators should have lower presedence than Arthmatic, but still be left associative*)
(*And and Ors should be right assiciative to make addressing expressions quicker.*)


%start <Utils.prog> prog

%%

prog:
  |e = expr EOF { e } 

expr:
  | IF; condition = expr; THEN; e1_expr = expr; ELSE; e2_expr = expr; (*Add IF THEN ELSE as an exprssion*)
    {If (condition, e1_expr, e2_expr)}
  | LET; x = VAR; EQ; e1 = expr; IN; e2 = expr
    {Let (x, e1, e2)}
  | FUN; y = VAR; ARROW; e = expr; (*Unike LET, args are required for funcitons.*)
    {Fun (y, e)}
  | e = expr1 { e }

%inline bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | GT { Gt }
  | LTE { Lte }
  | GTE { Gte }
  | AND { And }
  | OR { Or }
  | EQ { Eq }
  | NEQ { Neq }

expr1:
  | e1 = expr1; op = bop; e2 = expr1 { Bop (op, e1, e2) }
  | e = expr2; es = expr2* { mk_app e es }

expr2:
  | n = NUM { Num n }
  | v = VAR { Var v }
  | UNIT { Unit } (* Use constants to handle , unit, true, false, similar to how NUM and VAR are handeled.*)
  | TRUE { True }
  | FALSE { False }
  | LPAREN; e = expr; RPAREN { e } (*Handles cases for parenthesis to ensure they take presedence over all else*)