open Names
open Common 
exception Unbound_name of Names.Name.t
exception Ambiguous_name of Names.Name.t * Names.Name.t list
let third : ('a * 'b * 'c) -> 'c = fun (_, _, c) -> c
class process_names (pkg:Name.package) (store : Store.t) = object (self)
    val mutable open_modules : Name.path list = [[]]
    val mutable included_modules : Name.path list = [[]]
    val mutable local_binds : (Name.t * Store.action * Store.name_context list) list = []
    val mutable store : Store.t = store
    val mutable package : Names.Name.package = pkg 
    
    method private path_is_valid (path : Name.path) : Names.Query.t = Names.Query.package_query (fun pkg' -> pkg == pkg') (* TODO Create better logic *)
    
    method private push_local_bind (name:Name.t) (action:Store.action) (ctx:Store.name_context list) =
      local_binds <- (name, action, ctx) :: local_binds 
    
    method private pop_local_bind () =
      match local_binds with 
      | [] -> raise (Failure "No local binds to pop")
      | (name, action, ctx) :: rest -> 
          local_binds <- rest ; (name, action, ctx)
    method open_module name = 
        let (_, path, _) = Name.parse_name name in
        (open_modules <- path :: open_modules) ; name 
    method include_module (name : Name.t) : Name.t = 
      (included_modules <- snd3 (Name.parse_name name) :: included_modules) ; name
    method local_bind : 'a . ?ctx:Store.name_context list -> Name.t -> Store.action -> (Name.t -> 'a) -> 'a = 
        (fun ?(ctx = Names.any_context) name action f -> 
            self#push_local_bind name action ctx ;
            let result = f name in
            let _ = self#pop_local_bind () in
            result)
    method private get_local_bind : ?ctx:(Store.name_context list) -> Name.t -> (Name.t * Store.action * Store.name_context list) option = 
        (fun ?(ctx = Names.any_context) name -> 
            let rec find_in_binds binds = match binds with 
              | [] -> None
              | (n, action, c) :: rest -> 
                  if n = name && (List.exists (fun x -> List.mem x ctx) c) then 
                    Some ((n, action, c))
                  else  
                    (find_in_binds rest)
                  
            in
            find_in_binds local_binds)

    method private create_action (ctx:Store.name_context) (base : string) : string = match ctx with
        | Store.Value -> String.lowercase_ascii base
        | Store.Type -> String.lowercase_ascii base
        | Store.Constructor -> String.capitalize_ascii base
        | Store.Structure -> String.capitalize_ascii base
        | Store.Signature -> String.capitalize_ascii base
        | Store.Variable -> String.lowercase_ascii base
        
    method private run_action : ctx:Store.name_context -> Store.action -> Name.t -> Name.t = 
        (fun ~ctx action name -> match action with 
            | Store.Noted -> Name.make_name ~package:(fst3 (Name.parse_name name)) ~path:(snd3 (Name.parse_name name)) ~root:(self#create_action ctx (trd3 (Name.parse_name name)))
            | Store.Exact s -> Name.make_name ~package:(fst3 (Name.parse_name name)) ~path:(snd3 (Name.parse_name name)) ~root:s
            | Store.Change f -> f name
            | Store.NoChange -> name) 
    method process_name ?(ctx=Names.any_context) (name : Name.path * Name.name) : Name.t =
        match self#get_local_bind ~ctx (Name.make_name ~package:package ~path:(fst name) ~root:(snd name)) with
        | Some (n, action, ctx' :: _) -> self#run_action ~ctx:ctx' action n
        | None ->
            let name_question = Query.name_query ((==) @@ snd name) in
            let path_question : Query.t = (self#path_is_valid (fst name)) in
            let context_question = Query.context_query (fun e -> List.mem e ctx) in
            let combined_question = Query.query_and (Query.query_and name_question path_question) context_question in
            let has_name, _ = Query.select_query ~query:combined_question ~store:store in 
            match Store.entries has_name with
             [] -> let 
              action = self#create_action (List.hd ctx) (snd name) in 
              let new_name = Name.make_name ~package:package ~path:(fst name) ~root:action in
              let process_name_f ~(f:string) ~(n:Name.t) = let 
                _, path, root = Name.parse_name n 
              in Name.make_name ~package:package ~path:path ~root:f
              self#local_bind new_name (Store.Change (fun n -> action )) (fun _ -> new_name)
            | [entry] -> self#run_action ~ctx:(List.hd ctx) entry.action entry.name
            | entries -> raise (Ambiguous_name (Name.make_name ~package:package ~path:(fst name) ~root:(snd name), List.map (fun (e:Store.entry) -> e.name) entries))
        | _ -> assert false
    method name_to_idx (name : Name.t) : string list = let 
        packageName, path, root = Name.parse_name name 
    in path @ [root]

end



