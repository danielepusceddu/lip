{
open Parser
}

let white = [' ' '\t']+

(* define identifiers for variables
   as non-empty sequences of letters *)
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }

  | "0" { ZERO }
  | "iszero" { ISZERO }
  | "succ" { SUCC }
  | "pred" { PRED }

  | "let" { LET }
  | "=" { EQ }
  | "in" { IN }
  
  | id { ID (Lexing.lexeme lexbuf) } (* needs to be last! *)
  | eof { EOF }
