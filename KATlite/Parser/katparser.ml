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

open Parsing;;
let _ = parse_error;;
# 2 "Parser/katparser.mly"
open Ast
# 22 "Parser/katparser.ml"
let yytransl_const = [|
  259 (* ZERO *);
  260 (* ONE *);
  261 (* PLUS *);
  262 (* TIMES *);
  263 (* STAR *);
  264 (* NOT *);
  265 (* LPAREN *);
  266 (* RPAREN *);
  267 (* EQ *);
  268 (* LE *);
  269 (* IMP *);
  270 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* ACT *);
  258 (* TST *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\006\000\006\000\004\000\
\004\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\002\000\002\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\002\000\002\000\002\000\003\000\003\000\001\000\
\003\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\004\000\005\000\006\000\007\000\
\000\000\000\000\018\000\000\000\000\000\000\000\019\000\000\000\
\020\000\000\000\012\000\000\000\001\000\000\000\000\000\011\000\
\000\000\000\000\000\000\000\000\002\000\003\000\008\000\000\000\
\000\000\000\000\000\000\017\000"

let yydgoto = "\004\000\
\011\000\015\000\017\000\012\000\027\000\014\000"

let yysindex = "\002\000\
\105\255\105\255\105\255\000\000\000\000\000\000\000\000\000\000\
\105\255\105\255\000\000\012\255\055\255\035\255\000\000\009\255\
\000\000\051\255\000\000\067\255\000\000\105\255\105\255\000\000\
\105\255\105\255\255\254\105\255\000\000\000\000\000\000\096\255\
\255\254\087\255\087\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\065\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\255\000\000\000\000\000\000\000\000\073\255\
\041\255\006\255\021\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\021\000\255\255\077\000"

let yytablesize = 114
let yytable = "\013\000\
\016\000\013\000\001\000\002\000\003\000\024\000\009\000\019\000\
\020\000\005\000\006\000\007\000\008\000\022\000\023\000\024\000\
\009\000\010\000\014\000\014\000\032\000\033\000\029\000\034\000\
\035\000\021\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\015\000\015\000\013\000\013\000\013\000\013\000\013\000\
\013\000\010\000\010\000\010\000\010\000\010\000\010\000\028\000\
\036\000\010\000\010\000\010\000\010\000\010\000\010\000\005\000\
\006\000\007\000\008\000\022\000\023\000\024\000\009\000\010\000\
\030\000\025\000\026\000\005\000\006\000\007\000\008\000\022\000\
\023\000\024\000\009\000\010\000\031\000\009\000\016\000\018\000\
\000\000\000\000\009\000\009\000\009\000\009\000\009\000\005\000\
\006\000\007\000\008\000\022\000\023\000\024\000\009\000\010\000\
\005\000\006\000\007\000\008\000\000\000\023\000\024\000\009\000\
\010\000\005\000\006\000\007\000\008\000\000\000\000\000\000\000\
\009\000\010\000"

let yycheck = "\001\000\
\002\000\003\000\001\000\002\000\003\000\007\001\008\001\009\000\
\010\000\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\013\001\014\001\022\000\023\000\014\001\025\000\
\026\000\014\001\028\000\001\001\002\001\003\001\004\001\005\001\
\006\001\013\001\014\001\009\001\010\001\011\001\012\001\013\001\
\014\001\001\001\002\001\003\001\004\001\005\001\006\001\013\001\
\028\000\009\001\010\001\011\001\012\001\013\001\014\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\014\001\011\001\012\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\005\001\014\001\003\000\
\255\255\255\255\010\001\011\001\012\001\013\001\014\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\001\001\002\001\003\001\004\001\255\255\006\001\007\001\008\001\
\009\001\001\001\002\001\003\001\004\001\255\255\255\255\255\255\
\008\001\009\001"

let yynames_const = "\
  ZERO\000\
  ONE\000\
  PLUS\000\
  TIMES\000\
  STAR\000\
  NOT\000\
  LPAREN\000\
  RPAREN\000\
  EQ\000\
  LE\000\
  IMP\000\
  EOL\000\
  "

let yynames_block = "\
  ACT\000\
  TST\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 29 "Parser/katparser.mly"
              ( _1 )
# 142 "Parser/katparser.ml"
               : Ast.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 32 "Parser/katparser.mly"
           ( _1 )
# 149 "Parser/katparser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'equation) in
    Obj.repr(
# 35 "Parser/katparser.mly"
               ( _1 )
# 156 "Parser/katparser.ml"
               : Ast.equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "Parser/katparser.mly"
                    ( Act _1 )
# 163 "Parser/katparser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "Parser/katparser.mly"
                    ( Tst _1 )
# 170 "Parser/katparser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "Parser/katparser.mly"
                    ( Zero )
# 176 "Parser/katparser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "Parser/katparser.mly"
                    ( One )
# 182 "Parser/katparser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 42 "Parser/katparser.mly"
                       ( _2 )
# 189 "Parser/katparser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 43 "Parser/katparser.mly"
                    ( Plus [_1; _3] )
# 197 "Parser/katparser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 44 "Parser/katparser.mly"
                    ( Times [_1; _3] )
# 205 "Parser/katparser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 45 "Parser/katparser.mly"
                    ( Star _1 )
# 212 "Parser/katparser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 46 "Parser/katparser.mly"
             ( Not _2 )
# 219 "Parser/katparser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 47 "Parser/katparser.mly"
                          ( Times [_1; _2] )
# 227 "Parser/katparser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 50 "Parser/katparser.mly"
                 ( Eq (_1, _3) )
# 235 "Parser/katparser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 51 "Parser/katparser.mly"
                 ( Le (_1, _3) )
# 243 "Parser/katparser.ml"
               : 'equation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'equation) in
    Obj.repr(
# 54 "Parser/katparser.mly"
                         ( [_1] )
# 250 "Parser/katparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 55 "Parser/katparser.mly"
                         ( _1 :: _3 )
# 258 "Parser/katparser.ml"
               : 'formula))
(* Entry formula_main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry term_main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry equation_main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let formula_main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.formula)
let term_main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Ast.term)
let equation_main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : Ast.equation)
