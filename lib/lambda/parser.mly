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

term:
  | LAMBDA; x = NAME; PERIOD; t = term { Parsing.locate ~loc:(Parsing.mk_location $startpos $endpos) (Abs (x, t)) }
  | e = app_term { e } ;

app_term:
  | e = var_term; es = many_var_terms { List.fold_left (fun f a -> Parsing.locate ~loc:(Parsing.mk_location $startpos $endpos) (App(f, a))) e es }

many_var_terms:
  | (* empty *) { [] }
  | e = var_term; es = many_var_terms { e :: es }

var_term:
  | x = NAME { Parsing.locate ~loc:(Parsing.mk_location $startpos $endpos) (Var x) }
  | LPAREN; e = term; RPAREN { e } ;

add_location(X):
  x = X
  { Parsing.locate ~loc:(Parsing.mk_location $startpos $endpos) x }
