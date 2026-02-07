%token <string> NAME
%token LAMBDA
%token EQUALS
%token PERIOD
%token LPAREN
%token RPAREN
%token EOF

%{
open Syntax
open Common
%}

%start <prog> prog
%%

prog:
  | EOF { [] }
  | e = expr; p = prog { e :: p } ;

expr:
  | x = NAME; EQUALS; t = term { Decl (x, t) }
  | t = term { Term t }

term: x = add_location(plain_term) { x }
plain_term:
  | LAMBDA; x = NAME; PERIOD; t = term { Abs (x, t) }
  | e = plain_app_term { e } ;

app_term: add_location(plain_app_term) { $1 }
plain_app_term:
  | e = plain_var_term { e }
  | e1 = app_term; e2 = var_term { App (e1, e2) } ;

var_term: add_location(plain_var_term) { $1 }
plain_var_term:
  | x = NAME { Var x }
  | LPAREN; e = plain_term; RPAREN { e } ;

add_location(X):
  x = X
  { Parsing.locate ~loc:(Parsing.mk_location $startpos $endpos) x }
