%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token NOT
%token AND
%token OR
%token EOF

(* priorities of the tokens
   from lower to higher
   we use "ELSE" to define the priority of the if construct
   because the only possible ambiguity with the if is
   whether the expr after ELSE "belongs" to the ELSE or to the next operator
   (see production rules for expr below) *)
%nonassoc ELSE
%nonassoc OR
%nonassoc AND
%nonassoc NOT

%start <boolExpr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | NOT; e = expr; { Not(e) }
  | e1 = expr; AND; e2 = expr; { And(e1, e2) }
  | e1 = expr; OR; e2 = expr; { Or(e1, e2) }
  | LPAREN; e=expr; RPAREN {e}
;

