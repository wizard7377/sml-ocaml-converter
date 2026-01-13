open Cmdliner
open Cmdliner.Term.Syntax
include Cmd_common_options
include Process
type path = Fpath.t

exception Dir_exists of path 
exception Input_output_same_dir of path
exception Dir_create_error of path 
let path_to_string (p : path) : string =
  Fpath.to_string p
let string_to_path (s : string) : path =
  Fpath.v s
let convert_file ~(input_files : path list) ?(output_file : path option)
    ~(options : Common.options) : int =
  let input_files'' =
    match input_files with [] -> Common.StdIn | _ -> Common.File (List.map path_to_string input_files)
  in
  let output_target =
    match output_file with
    | None -> Common.Silent
    | Some p -> Common.FileOut (path_to_string p)
  in
  let cfg = Common.mkOptions
    ~input_file:input_files''
    ~output_file:output_target
    ~verbosity:(Common.get_verbosity options)
    ~conversions:(Common.get_conversions options)
    ~concat_output:(Common.get_concat_output options)
    ~force:(Common.get_force options)
    ()
  in
  let process = new process cfg in
  let res = process#run input_files'' in
  let _ = res in
  res


let is_directory (path : path) : bool =
  let open Bos in
  match OS.Dir.exists path with
  | Error _ -> false
  | Ok b -> b
let rec list_contents_rec (dir : path) : path list =
  let open Bos in
  let d = dir in
  match OS.Dir.contents ~dotfiles:true ~rel:true d with
  | Error e ->
      []
  | Ok files ->
      (* Filter for subdirectories and make paths absolute before recursing *)
      let subdirs = List.filter (fun f -> is_directory (Fpath.(//) d f)) files in
      let subdir_contents = List.concat_map (fun subdir ->
        let subdir_files = list_contents_rec (Fpath.(//) d subdir) in
        (* Prepend subdir name to each file from the subdirectory *)
        List.map (fun f -> Fpath.(//) subdir f) subdir_files
      ) subdirs in
      let res = List.(files @ subdir_contents) in
      let res2 = List.sort_uniq Fpath.compare res in
      res2

let create_dir (path : path) : unit =
  let open Bos in
  match OS.Dir.create path with
  | Error e ->
      raise (Dir_create_error path)
  | Ok _ -> ()
let copy_file (src : path) (dst : path) : unit =
  (* Check if it's a symlink and skip it *)
  let src_str = Fpath.to_string src in
  let lstat = Unix.lstat src_str in
  match lstat.st_kind with
  | Unix.S_LNK ->
      (* Skip symlinks *)
      ()
  | _ ->
      match Bos.OS.File.read src with
      | Ok contents ->
          let _ = Bos.OS.File.write dst contents in
          ()
      | Error (`Msg msg) ->
          Printf.eprintf "Error copying file %s: %s\n" (Fpath.to_string src) msg;
          ()
(* Directory, normal, source *)
let partition_files (files : path list) : path list * path list * path list =
  let (dirs, rest) = List.partition is_directory files in
  let (source_files, normal_files) = List.partition (fun p ->
    let ext = Fpath.get_ext p in
    ext = ".sml" || ext = ".sig" || ext = ".fun"
  ) rest in
  (dirs, normal_files, source_files)

let get_priority = function 
  | ".sig" -> 3
  | ".fun" -> 2
  | ".sml" -> 1
  | _ -> 0
let order_files (input_path0 : path) (input_path1 : path) : int =
  let (base0) = (Fpath.rem_ext input_path0, Fpath.get_ext input_path0) in
  let (base1) = (Fpath.rem_ext input_path1, Fpath.get_ext input_path1) in
  let ext0 = Fpath.get_ext input_path0 in
  let ext1 = Fpath.get_ext input_path1 in
  match compare base0 base1 with
  | 0 -> compare (get_priority ext0) (get_priority ext1)
  | n -> n
let process_sml_files (input_path:path) (output_path:path) (sml_files:path list) (options:Common.options) : (int * int) =
  let files = List.map (Fpath.rem_ext) sml_files in
  let groups = List.sort_uniq Fpath.compare files in
  let res = List.map (fun f ->
    let sml_file_candidates = [
      Fpath.add_ext ".sig" f;
      Fpath.add_ext ".fun" f;
      Fpath.add_ext ".sml" f;
    ] in
    let existing_files' = List.filter (fun p -> List.mem p sml_files) sml_file_candidates in
    let existing_files = List.rev @@ List.sort (order_files) existing_files' in
    if existing_files = [] then
      0
    else
      (* Make paths absolute by joining with input_path *)
      let existing_files_abs = List.map (fun p -> Fpath.(//) input_path p) existing_files in
      let output_file = Fpath.add_ext ".ml" (Fpath.(//) output_path f) in
      let status = convert_file ~input_files:existing_files_abs ~output_file ~options in
      status
  ) groups in
  let failures = List.fold_left (fun acc x -> if x <> 0 then acc + 1 else acc) 0 res in

  (failures, List.length res)
let convert_group ~(input_dir : path) ~(output_dir : path)
    ~(options : Common.options) : int =
  let r = Result.value ~default:true @@ Bos.OS.Dir.exists output_dir in
  (if r then
    if Common.get_force options then
      if input_dir == output_dir then
        let () = Printf.eprintf "Input and output directories cannot be the same.\n" in
        exit 1
      else
      let _ = Bos.OS.Dir.delete ~recurse:true output_dir in
      ()
    else
      let () = Printf.eprintf "Output directory %s already exists. Use --force to overwrite.\n" (Fpath.to_string output_dir) in
      exit 1
    ) ;
  let _ = Bos.OS.Dir.create output_dir in
  let all_files = list_contents_rec input_dir in
  (* Make paths absolute for partition_files by joining with input_dir *)
  let all_files_abs = List.map (fun f -> Fpath.(//) input_dir f) all_files in
  let (dirs, normal_files, source_files) = partition_files all_files_abs in
  (* Convert back to relative paths *)
  let to_relative p = match Fpath.rem_prefix input_dir p with Some rel -> rel | None -> p in
  let dirs_rel = List.map to_relative dirs in
  let normal_files_rel = List.map to_relative normal_files in
  let source_files_rel = List.map to_relative source_files in
  let _ = List.iter (fun d -> create_dir (Fpath.(//) output_dir d)) dirs_rel in
  let _ = List.iter (fun f -> copy_file (Fpath.(//) input_dir f) (Fpath.(//) output_dir f)) normal_files_rel in
  let (failures, total) = process_sml_files input_dir output_dir source_files_rel options in
  let () = Printf.printf "Conversion complete: %d successes, %d failures %d total.\n" (total - failures) failures total in
  if failures = 0 then 0 else 1