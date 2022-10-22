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

(* tokens for declarations *)
(* If an OCaml type t is present, then these tokens are considered 
   to carry a semantic value of type t... *)
%token <string> ID
%token LET
%token SEMICOLON
%token EQ
%token IN

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

%start <boolProg> prog

%%

prog:
  | d = declaration; e = expr; EOF { (d,e) }
  | e = expr; EOF { ([],e) }
;

binding:
  | x = ID; EQ; e = expr; { (x,e) }
;

binding_list:
  | h = binding; SEMICOLON; t = binding_list; { h::t }
  | b = binding; IN; { [b] }
;

declaration:
  | LET; d = binding_list; { d }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | NOT; e = expr; { Not(e) }
  | e1 = expr; AND; e2 = expr; { And(e1, e2) }
  | e1 = expr; OR; e2 = expr; { Or(e1, e2) }
  | LPAREN; e=expr; RPAREN {e}
  | x = ID { Var x }
;
