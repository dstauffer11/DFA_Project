{
  open Regexparser
}
let alphanumeric = ['a'-'z' 'A'-'Z' '0'-'9']*
rule token = parse
  | [' ' '\t']  { token lexbuf }
  | ['\n'] { EOL }
  | ['a'-'z'] alphanumeric as id { CHAR id }
  | ['A'-'Z'] alphanumeric as id { CHAR id }
  | '*'         { STAR }
  | '.'         { CONCAT }
  | '+'         { OR }
  | ' '         { EPSILON }
  | "Emptyset"  { EMPTYSET }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | eof         { EOL }
