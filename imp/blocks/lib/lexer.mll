{
open Parser
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']+

rule read =
  parse
  | white { read lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "skip" { SKIP }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { PRODUCT }
  | "<=" { LESSEQUAL }
  | ":=" { ASSIGN }
  | "=" { EQUAL }
  | ";" { SEMICOLON }

  | "{" { BEGIN_BLOCK }
  | "}" { END_BLOCK }
  | "int" { INT_TYPE }
  | "bool" { BOOL_TYPE }

  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | id { VAR (Lexing.lexeme lexbuf) }
  | eof { EOF }