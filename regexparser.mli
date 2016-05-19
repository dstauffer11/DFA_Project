type token =
  | CHAR of (char)
  | STAR
  | CONCAT
  | OR
  | EPSILON
  | EMPTYSET
  | LPAREN
  | RPAREN
  | EOL

val regex_main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Definitions.regex
