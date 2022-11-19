%{
open Ast
%}

%token <string> VAR
%token ABS
%token DOT
%token LPAREN
%token RPAREN
%token EOF
%token TRU
%token FLS
%token IFT
%token PAIR
%token FST
%token SND
%token <int> NUM
%token SCC
%token ADD
%token ID
%token AND
%token OMEGA

%start <term> prog

%nonassoc DOT ABS
%nonassoc LPAREN VAR AND TRU FLS IFT PAIR FST SND NUM SCC ADD ID OMEGA
%left APP
%%

prog:
  | t = term; EOF { t }
;

term:
  | x = VAR { Var x }
  | ABS; x = VAR; DOT; t = term { Abs(x,t) }
  | LPAREN; t=term; RPAREN { t }
  | TRU; { t_tru }
  | FLS; { t_fls }
  | IFT; { t_ift }
  | PAIR; { t_pair }
  | FST; { t_fst }
  | SND; { t_snd }
  | n = NUM { t_nat n }
  | SCC { t_scc }
  | ADD { t_add }
  | ID { t_id }
  | AND { t_and }
  | OMEGA { t_omega }
  | t1=term; t2=term { App(t1,t2) } %prec APP
;
