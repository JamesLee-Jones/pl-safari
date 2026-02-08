open Common

type name = string

type term = term' Parsing.located
and term' = Abs of name * term | App of term * term | Var of name

type expr = Decl of name * term | Term of term
type prog = expr list
