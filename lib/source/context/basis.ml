let create_constructors (names : (string list * string * int) list) : Info.t =
  let entries =
    List.map (fun (path, root, arity) ->
        let name = { Info.path = path; root = root } in
        let info = Info.ConstructorInfo { arity = Some arity } in
        (name, info)
    ) names
  in
  Info.create entries

(*
eqtype unit 	General
eqtype int 	Int
eqtype word 	Word
type real 	Real
eqtype char 	Char
eqtype string 	String
type substring 	Substring
type exn 	General
eqtype 'a array 	Array
eqtype 'a vector 	Vector
eqtype 'a ref 	primitive
datatype bool = false | true 	primitive
datatype 'a option = NONE | SOME of 'a 	Option
datatype order = LESS | EQUAL | GREATER 	General
datatype 'a list = nil | :: of ('a * 'a list) 	primitive
exception Chr
exception Div
exception Domain
exception Empty
exception Fail of string
exception Match
exception Option
exception Overflow
exception Size
exception Span
exception Subscript

*)
let basis_context = create_constructors [
  [], "nil", 0;
  [], "SOME", 1;
  [], "NONE", 0;
  [], "true", 0;
  [], "false", 0;
  [], "LESS", 0;
  [], "EQUAL", 0;
  [], "GREATER", 0;
  [], "::", 2;
  [], "Chr", 0;
  [], "Div", 0;
  [], "Domain", 0;
  [], "Empty", 0;
  [], "Fail", 1;
  [], "Match", 0;
  [], "Option", 0;
  [], "Overflow", 0;
  [], "Size", 0;
  [], "Span", 0;
  [], "Subscript", 0;
  ["Array"], "array", 1;
  ["Vector"], "vector", 1;
  
]
