let is_all_uppercase s =
  String.for_all (fun c -> Char.uppercase_ascii c = c) s

let transform_constructor name =
  if String.length name = 0 then name
  else if String.ends_with ~suffix:"_" name then
    (* B_ -> B__ *)
    let result = name ^ "_" in
    Printf.eprintf "DEBUG: %s (trailing_) -> %s\n%!" name result;
    result
  else if Char.uppercase_ascii name.[0] = name.[0] then
    (* Already starts with uppercase *)
    if is_all_uppercase name && String.length name > 1 then
      (* SOME -> Some *)
      let result = String.capitalize_ascii (String.lowercase_ascii name) in
      Printf.eprintf "DEBUG: %s (all_caps) -> %s\n%!" name result;
      result
    else
      (* Foo -> Foo *)
      Printf.eprintf "DEBUG: %s (proper_case) -> %s\n%!" name name;
      name
  else
    (* Lowercase: a -> A_, some -> Some_ *)
    let result = String.capitalize_ascii name ^ "_" in
    Printf.eprintf "DEBUG: %s (lowercase) -> %s\n%!" name result;
    result

let transform_to_lowercase name =
  if String.length name = 0 then name
  else if Char.lowercase_ascii name.[0] = name.[0] then
    (* Already lowercase *)
    name
  else
    (* Uppercase: SOME -> some_, Foo -> foo_ *)
    String.uncapitalize_ascii name ^ "_"
