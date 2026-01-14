let comment_re =
  Re.compile
    (Re.seq
       [
         Re.str "[";
         Re.rep1 (Re.str "@");
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

let clean_comment =
  Re.replace_string (Re.compile @@ Re.alt [ Re.str "(*"; Re.str "*)" ]) ~by:""

let comment_replace : Re.Group.t -> string =
 fun grp ->
  let group_names = Re.group_names comment_re in
  match List.assoc_opt "body" group_names with
  | Some body -> (
      Re.Group.get_opt grp body |> function
      | Some body ->
          let res = "(*" ^ clean_comment body ^ "*)" in
          res
      | None -> "")
  | None -> ""

let polish_comments (input : string) : string =
  Re.replace ?all:(Some true) comment_re ~f:comment_replace input

let polish_format (input : string) : string = input
let polish (input : string) = input |> polish_comments |> polish_format
