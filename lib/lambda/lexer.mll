{
open Lexing
open Parser
exception SyntaxError of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let prime = '\''
let name = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9'] prime*

rule read =
  parse
  | white { read lexbuf }
  | newline { new_line lexbuf; read lexbuf }
  | name { NAME (Lexing.lexeme lexbuf) }
  | "\\" { LAMBDA }
  | "=" { EQUALS }
  | "." { PERIOD }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
