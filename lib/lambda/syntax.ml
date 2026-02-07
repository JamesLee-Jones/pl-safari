open Base
open Common

type name = string [@@deriving sexp, compare]

type term = term' Parsing.located

and term' = Abs of name * term | App of term * term | Var of name
[@@deriving sexp, compare]

type expr = Decl of name * term | Term of term [@@deriving sexp, compare]
type prog = expr list [@@deriving sexp_of, compare]
