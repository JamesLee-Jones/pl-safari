open Common

type index = int
type indexed_name = index * Syntax.name

type term = term' Parsing.located

and term' =
  | Abs of Syntax.name * term
  | App of term * term
  | Var of indexed_name

type expr = Decl of indexed_name * term | Term of term
type prog = expr list
