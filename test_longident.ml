open Ppxlib

let test1 = Longident.unflatten ["::"]
let test2 = Longident.unflatten ["[]"]

let () =
  Printf.printf "test1: %s\n" (match test1 with Some x -> Longident.name x | None -> "None");
  Printf.printf "test2: %s\n" (match test2 with Some x -> Longident.name x | None -> "None")
