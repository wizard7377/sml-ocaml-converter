

type path = string list

type name_info = ..
type name_info += ConstructorInfo of {
    arity : int option ;
}
type name_info += ModuleInfo of {
    maps_to : path option
}

type name = {
    path : path ;
    root : string ;
}
type t = (name, name_info) Hashtbl.t

let create (entries : (name * name_info) list) : t =
  let table = Hashtbl.create (List.length entries) in
  List.iter (fun (n, info) -> Hashtbl.add table n info) entries;
  table
let merge (t1 : t) (t2 : t) : t =
    let merged = Hashtbl.copy t1 in
    Hashtbl.iter (fun k v -> Hashtbl.replace merged k v) t2;
    merged
let find ~(ctx : t) ~(opened : path list) ~(root : string) : name_info list =
    let results = ref [] in
    Hashtbl.iter (fun n info ->
        if List.mem n.path opened && n.root = root then
        results := info :: !results
    ) ctx;
    !results