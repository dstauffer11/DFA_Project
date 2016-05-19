%{
  open Definitions
%}

%token <char> CHAR
%token STAR
%token CONCAT OR
%token EPSILON EMPTYSET
%token LPAREN RPAREN
%token EOL


%right EMPTYSET EPSILON EOL
%left OR
%left CONCAT LPAREN
%nonassoc STAR
%right CHAR

%start regex_main
%type <Definitions.regex> regex_main


%%

regex_main:
  | CHAR regex_main               { Char $1 }
  | regex_main STAR               { Star $1 }
  | regex_main CONCAT regex_main  { Concat [$1; $3] }
  | regex_main OR regex_main      { Or [$1; $3] }
  | EPSILON                       { Epsilon }
  | EMPTYSET                      { Emptyset }
  | LPAREN regex_main RPAREN      { $2 }
  | regex_main EOL                { $1 }
;