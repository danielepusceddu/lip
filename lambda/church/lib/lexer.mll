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
  | "fun" { ABS }
  | "tru" { TRU }
  | "fls" { FLS }
  | "ift" { IFT }
  | "pair" { PAIR }
  | "fst" { FST }
  | "snd" { SND }
  | "id" { ID }
  | "and" { AND }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | "scc" { SCC }
  | "add" { ADD }
  | "omega" { OMEGA }
  | id { VAR (Lexing.lexeme lexbuf) }
  | "." { DOT }
  | eof { EOF }
