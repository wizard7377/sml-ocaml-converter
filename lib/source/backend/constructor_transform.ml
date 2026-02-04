let is_all_uppercase s =
  String.for_all (fun c -> not (Char.lowercase_ascii c <> c)) s

let transform_constructor name =
  if String.length name = 0 then name
  else if String.ends_with ~suffix:"_" name then
    (* B_ -> B__ *)
    name ^ "_"
  else if Char.uppercase_ascii name.[0] = name.[0] then
    (* Already starts with uppercase *)
    if is_all_uppercase name && String.length name > 1 then
      (* SOME -> Some *)
      String.capitalize_ascii (String.lowercase_ascii name)
    else
      (* Foo -> Foo *)
      name
  else
    (* Lowercase: a -> A_, some -> Some_ *)
    String.capitalize_ascii name ^ "_"

let transform_to_lowercase name =
  if String.length name = 0 then name
  else if Char.lowercase_ascii name.[0] = name.[0] then
    (* Already lowercase *)
    name
  else
    (* Uppercase: SOME -> some_, Foo -> foo_ *)
    String.uncapitalize_ascii name ^ "_"
