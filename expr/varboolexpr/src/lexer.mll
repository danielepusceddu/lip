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
  | eof { EOF }
  (* for declarations *)
  | "let" { LET }
  | "in" { IN }
  | ";" { SEMICOLON }
  | "=" { EQ }
  | id { ID (Lexing.lexeme lexbuf) } (* needs to be last! *)
  (* Lexing.lexeme lexbuf returns the string matched by the regular expression. *)

