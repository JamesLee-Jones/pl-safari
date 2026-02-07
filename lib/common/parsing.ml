type location = Nowhere | Location of Lexing.position * Lexing.position

type 'a located = {
  data : 'a;
  loc : (location[@sexp.opaque] [@compare.ignore]);
}
[@@deriving sexp, compare]

let mk_location loc1 loc2 = Location (loc1, loc2)
let locate ?(loc = Nowhere) (data : 'a) : 'a located = { data; loc }
