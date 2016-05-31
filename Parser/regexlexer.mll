{
  open Regexparser
}
rule token = parse
  | ['\n'] { EOL }
  | ['a'-'z'] as id { CHAR id }
  | ['A'-'Z'] as id { CHAR id }
  | '*'         { STAR }
  | '.'         { CONCAT }
  | '+'         { OR }
  | ' '         { EPSILON }
  | "Emptyset"  { EMPTYSET }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | eof         { EOL }
