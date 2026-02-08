open Common

type index = int
type indexed_name = index * Syntax.name

exception Temp of string

type term = term' Parsing.located

and term' =
  | Abs of Syntax.name * term
  | App of term * term
  | Var of indexed_name

type expr = Decl of indexed_name * term | Term of term
type prog = expr list

let pp_name ppf = Format.fprintf ppf "%i"

let rec pp_term ppf (t : term) = pp_term' ppf t.data

and pp_term' ppf = function
  | Abs (x, t) -> Format.fprintf ppf "(\\%s.%a)" x pp_term t
  | App (t1, t2) -> Format.fprintf ppf "%a %a" pp_term t1 pp_term t2
  | Var (_, x) -> Syntax.pp_name ppf x

let pp_expr ppf = function
  | Decl ((_, x), t) -> Format.fprintf ppf "%s = %a" x pp_term t
  | Term t -> pp_term ppf t

let pp_prog ppf = List.iter (pp_expr ppf)
