open Common
open Ir

(* [shift d ?(c=0) located_term] shifts all variables above the cutoff [c] by [d]. *)
let rec shift d ?(c = 0) (located_term : term) =
  let shifted =
    match located_term.data with
    | Var (index, name) ->
        if index < c then Var (index, name) else Var (index + d, name)
    | Abs (name, t) -> Abs (name, shift d ~c:(c + 1) t)
    | App (t1, t2) -> App (shift d ~c t1, shift d ~c t2)
  in
  Parsing.locate ~loc:located_term.loc shifted

(* [sub index replacement term] replaces any occurances of the index [index] with in [term] with [replacement]. *)
let rec sub index replacement (term : term) =
  let substituted =
    match term.data with
    | Var (i, name) ->
        if i = index then
          let { Parsing.data = repl; _ } = replacement in
          repl
        else Var (i, name)
    | Abs (x, t) -> Abs (x, sub (index + 1) (shift 1 replacement) t)
    | App (t1, t2) -> App (sub index replacement t1, sub index replacement t2)
  in
  Parsing.locate ~loc:term.loc substituted

(* [eval] evaluates a term, with the arguments indicating how the expression should be evaluated *)
let rec eval_term ?(strict = true) ?(deep = false) (located_term : term) =
  match located_term.data with
  | App (t1, t2) -> (
      let t2 = if strict then eval_term ~strict ~deep t2 else t2 in
      match t1.data with
      | Abs (_, t) ->
          eval_term ~strict ~deep (shift (-1) (sub 0 (shift 1 t2) t))
      | _ ->
          let t = eval_term ~strict ~deep t1 in
          eval_term ~strict ~deep (Parsing.locate ~loc:t1.loc (App (t, t2))))
  | Abs (x, t) ->
      if deep then
        Parsing.locate ~loc:located_term.loc
          (Abs (x, eval_term ~strict ~deep t))
      else located_term
  | _ -> raise (Temp "AHHH")

(* [eval] evaluates an expression, with the arguments indicating how the expression should be evaluated *)
let eval ?(strict = true) ?(deep = false) = function
  | Decl (_, t) -> eval_term ~strict ~deep t
  | Term t -> eval_term ~strict ~deep t
