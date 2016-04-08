%{
open Ast
%}

%token <string> ACT
%token <string> TST
%token ZERO ONE
%token PLUS TIMES STAR
%token NOT
%token LPAREN RPAREN
%token EQ LE
%token IMP
%token EOL

%right IMP         /* lowest precedence */
%nonassoc EQ LE
%left PLUS
%left TIMES ACT TST ZERO ONE LPAREN
%nonassoc NOT STAR /* highest precedence */

%start formula_main term_main equation_main  /* entry points */
%type <Ast.formula> formula_main
%type <Ast.equation> equation_main
%type <Ast.term> term_main

%%

formula_main:
  formula EOL { $1 }
;
term_main:
  term EOL { $1 }
;
equation_main:
  equation EOL { $1 }
;
term:
  | ACT             { Act $1 }
  | TST             { Tst $1 }
  | ZERO            { Zero }
  | ONE             { One }
  | LPAREN term RPAREN { $2 }
  | term PLUS term  { Plus [$1; $3] }
  | term TIMES term { Times [$1; $3] }
  | term STAR       { Star $1 }
  | NOT term { Not $2 }
  | term term %prec TIMES { Times [$1; $2] }
;
equation:
    term EQ term { Eq ($1, $3) }
  | term LE term { Le ($1, $3) }
;
formula:
    equation             { [$1] }
  | equation IMP formula { $1 :: $3 }
;
