type token =
  | ACT of (string)
  | TST of (string)
  | ZERO
  | ONE
  | PLUS
  | TIMES
  | STAR
  | NOT
  | LPAREN
  | RPAREN
  | EQ
  | LE
  | IMP
  | EOL

val formula_main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.formula
val term_main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.term
val equation_main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.equation
