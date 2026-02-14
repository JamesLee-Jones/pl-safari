type name = string

(** A lambda term consisting of variables, abstraction and application.*)
type term = Abs of name * term | App of term * term | Var of name

type located_term = term Common.Parsing.located

(** A top level expression is either a variable declaration of the form
    [name = lambda] or a lambda expression. *)
type expr = Decl of name * located_term | Term of located_term

type located_expr = expr Common.Parsing.located
type prog = located_expr list
