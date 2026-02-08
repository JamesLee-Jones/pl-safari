open Common
open Syntax
open Common.Error

let ( let* ) = Result.bind

let lookup_index loc x =
  let rec lookup_index index = function
    | [] -> Result.Error (Error (loc, "Error"))
    | c :: cs -> if x = c then Result.Ok index else lookup_index (index + 1) cs
  in
  lookup_index 0

let mk_indexed_name loc x ctx =
  let* index = lookup_index loc x ctx in
  Ok (index, x)

let rec term ctx { Parsing.data = t; Parsing.loc } =
  let* desugared =
    match t with
    | Abs (x, t) ->
        let* desugared = term (x :: ctx) t in
        Result.Ok (Ir.Abs (x, desugared))
    | App (t1, t2) ->
        let* t1_desugared = term ctx t1 in
        let* t2_desugared = term ctx t2 in
        Result.Ok (Ir.App (t1_desugared, t2_desugared))
    | Var x ->
        let* index = mk_indexed_name loc x ctx in
        Result.Ok (Ir.Var index)
  in
  Result.Ok (Parsing.locate ~loc desugared)
