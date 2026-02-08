open Common

type name = string

type term = term' Parsing.located
and term' = Abs of name * term | App of term * term | Var of name

type expr = Decl of name * term | Term of term
type prog = expr list

let pp_name ppf = Format.fprintf ppf "%s"

(* Printing Functions *)

let rec pp_term ppf (t : term) = pp_term' ppf t.data

and pp_term' ppf = function
  | Abs (x, t) -> Format.fprintf ppf "(\\%a.%a)" pp_name x pp_term t
  | App (t1, t2) -> Format.fprintf ppf "%a %a" pp_term t1 pp_term t2
  | Var x -> pp_name ppf x

let pp_expr ppf = function
  | Decl (x, t) -> Format.fprintf ppf "%a = %a" pp_name x pp_term t
  | Term t -> pp_term ppf t

let pp_prog ppf = List.iter (pp_expr ppf)
