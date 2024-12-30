%{
open Utils

let rec mk_app e = function 
  |[] -> e
  |x :: es -> mk_app (SApp(e, x)) es

%}

(*Make stuff to handle errors at some point*)

%token EOF
%token LET "let"
%token REC "rec"
%token IN "in"
%token COLLON ":"
%token FUN "fun" (*Expressions*)
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token LPAREN "("
%token RPAREN ")"
%token ARROW "->"
%token UNIT "()"
%token TRUE "true"
%token FALSE "false"
%token ADD "+" (*Operators*)
%token SUB "-"
%token MUL "*"
%token DIV "/"
%token MOD "mod"
%token LT "<"(*Comparisons*)
%token GT  ">"
%token LTE  "<="
%token GTE ">="
%token EQ "="
%token NEQ "<>"
%token AND "&&"
%token OR "||"
%token BOOLTY "bool"
%token INTTY "int"
%token UNITTY "unit"
%token ASSERT "assert"
%token <string> VAR
%token <int> NUM



%right ARROW 
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
  |ls = toplet* EOF { ls } 


toplet:
  | "let" x = VAR a = arg* ":" ty = ty "=" e = expr
    {{ is_rec = false; name = x; args = a; ty = ty; value = e }}
  | "let" "rec" f = VAR a = arg b = arg* ":" ty = ty "=" e = expr
    {{ is_rec = true; name = f; args = a :: b; ty = ty; value = e } }
  | error { failwith "Syntax error in toplet" }

arg:
|"(" x = VAR ":" ty = ty ")" { (x, ty) }
 
ty:
  | "unit" { UnitTy }
  | "int" { IntTy }
  | "bool" { BoolTy }
  | t1 = ty "->" t2 = ty { FunTy (t1, t2) }
  | "(" ty = ty ")" { ty }
 

expr:
  | "let" x = VAR a = arg* ":" ty = ty "=" e1 = expr "in" e2 = expr 
    { SLet { is_rec = false; name = x; args = a; ty = ty; value = e1; body = e2 } }

  | "let" "rec" f = VAR a = arg b = arg* ":" ty = ty "=" e1 = expr "in" e2 = expr
      { SLet { is_rec = true; name = f; args = a :: b ; ty = ty; value = e1; body = e2 } }

  | "if" e1 = expr "then" e2 = expr "else" e3 = expr { SIf (e1, e2, e3) }

  | "fun" a = arg b = arg* "->" e = expr { SFun ({arg = a; args = b; body = e}) } 

  | e = expr2 { e }

  | error { failwith "error in expr" }

%inline bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }

expr2:
  | e1 = expr2 op = bop e2 = expr2 { SBop (op, e1, e2) }
  | "assert" e = expr3 {SAssert (e)}
  | e = expr3 es = expr3* { mk_app e es }
  | error { failwith "Syntax error in expr2" }

expr3:
  | "()" { SUnit }
  | x = VAR { SVar x }
  | n = NUM { SNum n }
  | "true" { STrue }
  | "false" { SFalse }
  | "(" e = expr ")" { e }
  | error { failwith "Syntax error in expr3" }