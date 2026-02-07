open Syntax

let name ppf = Format.fprintf ppf "%s"

let rec term ppf (t : Syntax.term) = term' ppf t.data

and term' ppf = function
  | Abs (x, t) -> Format.fprintf ppf "(\\%a.%a)" name x term t
  | App (t1, t2) -> Format.fprintf ppf "%a %a" term t1 term t2
  | Var x -> name ppf x

let expr ppf = function
  | Decl (x, t) -> Format.fprintf ppf "%a = %a" name x term t
  | Term t -> term ppf t

let prog ppf = List.iter (expr ppf)
