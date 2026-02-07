open Format
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let round_trip prog =
  let buff = Buffer.create 16 in
  let ppf = formatter_of_buffer buff in

  Lambda.Print.prog ppf prog;
  pp_print_flush ppf ();
  let lexbuf = Lexing.from_string ~with_positions:true (Buffer.contents buff) in
  Lambda.Parser.prog Lambda.Lexer.read lexbuf

let examples =
  let open Lambda.Syntax in
  let open Common.Parsing in
  [
    [];
    [ Term (locate (Var "x")) ];
    [ Term (locate (Abs ("x", locate (Var "y")))) ];
    [ Term (locate (App (locate (Var "x"), locate (Var "y")))) ];
  ]

let%test_unit "print_parse_round_trip" =
  List.iter
    (fun prog -> [%test_eq: Lambda.Syntax.prog] (round_trip prog) prog)
    examples
