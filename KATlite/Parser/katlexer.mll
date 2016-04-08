{
open Katparser
}
let alphanumeric = ['a'-'z' 'A'-'Z' '0'-'9']*
rule token = parse
    [' ' '\t']     { token lexbuf }     (* skip blanks *)
  | ['\n'] { EOL }
  | ['a'-'z'] alphanumeric as id { ACT id }
  | ['A'-'Z'] alphanumeric as id { TST id }
  | '0'    { ZERO }
  | '1'    { ONE }
  | '+'    { PLUS }
  | '.'    { TIMES }
  | ';'    { TIMES }
  | '*'    { STAR }
  | '~'    { NOT }
  | '('    { LPAREN }
  | ')'    { RPAREN }
  | '='    { EQ }
  | '<'    { LE }
  | "->"   { IMP }
  | eof    { EOL }
