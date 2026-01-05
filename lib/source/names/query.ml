include Store
type t = Store.entry -> bool


let run_query ~query:query ~entry:entry : bool = 
  query entry

let select_query ~query:query ~store:store : Store.t * Store.t = 
  Store.select query store
let query_and f g x = f x && g x
let query_or f g x = f x || g x
let query_not f x = not (f x)
let query_xor f g x = (f x && not (g x)) || (not (f x) && g x)

let name_query f entry = let 
  (_, _, root) = Name.parse_name entry.name in
  f root
let context_query f = fun entry -> f entry.context
let path_query f entry = let 
  (_, path, _) = Name.parse_name entry.name in
  f path

let package_query f entry = let 
  (package, _, _) = Name.parse_name entry.name in
  f package
let action_query f = fun entry -> f entry.action

let yes_query = fun _ -> true
let no_query = fun _ -> false

