type location = Nowhere | Location of Lexing.position * Lexing.position
type 'a located = { data : 'a; loc : location }

let locate ?(loc = Nowhere) data = { data; loc }
