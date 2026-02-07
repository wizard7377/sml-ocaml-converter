let comment_re =
  Re.compile
    (Re.seq
       [
         Re.str "[";
         Re.group ?name:(Some "ats") (Re.rep1 (Re.str "@"));
         Re.str "sml.comment";
         Re.rep Re.space;
         Re.str "\"";
         Re.rep Re.space;
         Re.str "(*";
         Re.group ?name:(Some "body") (Re.shortest (Re.rep Re.any));
         Re.str "*)";
         Re.rep Re.space;
         Re.str "\"";
         Re.rep Re.space;
         Re.str "]";
       ])

let clean_comment (input : string) : string =
  let res0 =
    Re.replace_string
      (Re.compile @@ Re.alt [ Re.str "(*"; Re.str "*)" ])
      ~by:"" input
  in
  Scanf.unescaped res0

let comment_replace : Re.Group.t -> string =
 fun grp ->
  let group_names = Re.group_names comment_re in
  let ats_len =
    match List.assoc_opt "ats" group_names with
    | Some idx -> (
        match Re.Group.get_opt grp idx with
        | Some s -> String.length s
        | None -> 2)
    | None -> 2
  in
  match List.assoc_opt "body" group_names with
  | Some body -> (
      Re.Group.get_opt grp body |> function
      | Some body ->
          let comment = "(*" ^ clean_comment body ^ "*)" in
          (* Node-attached attributes (@ or @@) get a newline prefix so
             they appear on their own line instead of glued to the expression.
             Floating attributes (@@@) already have their own line from Pprintast. *)
          if ats_len < 3 then "\n" ^ comment
          else comment
      | None -> "")
  | None -> ""

let polish_comments (input : string) : string =
  Re.replace ?all:(Some true) comment_re ~f:comment_replace input

let polish_format (input : string) : string = input
let polish (input : string) = input |> polish_comments |> polish_format
