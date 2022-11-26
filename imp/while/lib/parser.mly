%{
open Ast
%}

%token <string> VAR
%token LPAREN
%token RPAREN
%token EOF
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token AND
%token OR
%token NOT
%token <int> NUM
%token PLUS
%token MINUS
%token PRODUCT
%token EQUAL
%token LESSEQUAL
%token ASSIGN
%token SEMICOLON
%token WHILE
%token DO
%token SKIP

%start <cmd> prog

%left SEMICOLON
%left ELSE DO
%left ASSIGN
%left AND OR
%left EQUAL LESSEQUAL
%left PLUS MINUS
%left PRODUCT
%left NOT
%%

prog:
  | c = cmd; EOF { c }
;

expr:
  | TRUE; { True }
  | FALSE; { False }
  | x = VAR; { Var(x) }
  | n = NUM; { Const(n) }
  | NOT; e = expr; { Not(e) }
  | e1 = expr; AND; e2 = expr; { And(e1, e2)}
  | e1 = expr; OR; e2 = expr; { Or(e1, e2) }
  | e1 = expr; PLUS; e2 = expr; { Add(e1, e2) }
  | e1 = expr; MINUS; e2 = expr; { Sub(e1, e2) }
  | e1 = expr; PRODUCT; e2 = expr; { Mul(e1, e2) }
  | e1 = expr; EQUAL; e2 = expr; { Eq(e1, e2) }
  | e1 = expr; LESSEQUAL; e2 = expr; { Leq(e1, e2) }
  | LPAREN; e = expr; RPAREN; { e }

cmd:
  | SKIP; { Skip }
  | x = VAR; ASSIGN; e = expr; { Assign(x, e) }
  | c1 = cmd; SEMICOLON; c2 = cmd; { Seq(c1,c2) }
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd; { If(e, c1, c2) }
  | WHILE; e = expr; DO; c = cmd; { While(e, c) }
