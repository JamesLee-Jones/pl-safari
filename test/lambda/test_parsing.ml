open Format

let assert_string_equal ~expected ~actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "Strings differ!\n\nEXPECTED:\n%s\n\nACTUAL:\n%s\n"
         expected actual)

let list_relative_directory dir =
  let full_dir = Filename.concat (Sys.getcwd ()) dir in
  if Sys.file_exists full_dir && Sys.is_directory full_dir then
    let file_names = Array.to_list (Sys.readdir full_dir) in
    List.map (fun fn -> Filename.concat full_dir fn) file_names
  else failwith (Printf.sprintf "%s" full_dir)

let test_directory test_fn dir = List.iter test_fn (list_relative_directory dir)

(* Test the round trip property of parsing and printing *)
let test_valid file_path =
  try
    (* Parse input file *)
    let ic = open_in file_path in
    let in_lexbuf = Lexing.from_channel ~with_positions:true ic in
    let in_tree = Lambda.Parser.prog Lambda.Lexer.read in_lexbuf in

    (* Print to buffer *)
    let in_buff = Buffer.create 16 in
    let in_ppf = formatter_of_buffer in_buff in
    Lambda.Print.prog in_ppf in_tree;

    (* Reparse *)
    let out_lexbuf =
      Lexing.from_string ~with_positions:false (Buffer.contents in_buff)
    in
    let out_tree = Lambda.Parser.prog Lambda.Lexer.read out_lexbuf in

    (* Reprint *)
    let out_buff = Buffer.create 16 in
    let out_ppf = formatter_of_buffer out_buff in
    Lambda.Print.prog out_ppf out_tree;

    (* Check printed results are equal *)
    assert_string_equal ~expected:(Buffer.contents in_buff)
      ~actual:(Buffer.contents out_buff)
  with e ->
    failwith
      (Printf.sprintf "Roundtrip failed for %s with %s" file_path
         (Printexc.to_string e))

(* Test that parsing fails with a syntax error *)
let test_syntax_error file_path =
  (* Parse input file *)
  let ic = open_in file_path in
  let in_lexbuf = Lexing.from_channel ~with_positions:true ic in
  try
    let _ = Lambda.Parser.prog Lambda.Lexer.read in_lexbuf in
    failwith "Expected syntax error"
  with Lambda.Lexer.SyntaxError _ -> ()

let%test_unit "test_round_trip" = test_directory test_valid "cases/valid"

let%test_unit "test_syntax_error" =
  test_directory test_syntax_error "cases/syntax_error"
