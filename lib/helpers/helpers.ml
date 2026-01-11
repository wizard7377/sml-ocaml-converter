module Builder = Ppxlib.Ast_builder.Make (struct
  let loc = Ppxlib.Location.none
end)

let empty_loc = Ppxlib.Location.none

module Attr = Attr
