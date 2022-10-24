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

%token PRED
%token SUCC
%token ZERO
%token ISZERO

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

%start <expr> prog

%%

prog:
  | e = boolexpr; EOF { BoolExpr e }
  | e = number; EOF { NatExpr e }
;

boolexpr:
  | TRUE { True }
  | FALSE { False }
  | IF; e1 = boolexpr; THEN; e2 = boolexpr; ELSE; e3 = boolexpr; { If(e1, e2, e3) }
  | NOT; e = boolexpr; { Not(e) }
  | e1 = boolexpr; AND; e2 = boolexpr; { And(e1, e2) }
  | e1 = boolexpr; OR; e2 = boolexpr; { Or(e1, e2) }
  | ISZERO; n = number; { IsZero(n) }
  | LPAREN; e=boolexpr; RPAREN {e}
;

number:
  | ZERO { Zero }
  | PRED; n = number; { Pred(n) }
  | SUCC; n = number; { Succ(n) }
