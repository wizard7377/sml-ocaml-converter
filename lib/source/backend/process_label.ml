include Helpers

type atag = string
type attr = Parsetree.attribute
type cite = Parsetree.payload
type 'a citer = 'a -> attr -> 'a

exception CommentNotFound

let rec get_all_comments (lexbuf : string) : (string * int * int) list =
  let comment_regex =
    Re.compile
      (Re.seq [ Re.str "(*"; Re.shortest (Re.rep Re.any); Re.str "*)" ])
  in
  let comments = Re.all comment_regex lexbuf in
  let process_comment_group : Re.Group.t -> string * int * int =
   fun group ->
    let comment_str = Re.Group.get group 0 in
    let start_pos = Re.Group.start group 0 in
    let end_pos = Re.Group.stop group 0 in
    (comment_str, start_pos, end_pos)
  in
  List.map process_comment_group comments

class process_label opts lexbuf =
  object (self)
    val options : Common.options = opts
    val mutable comments : (string * int * int) list = get_all_comments lexbuf

    val mutable lexbuf : string =
      lexbuf (* TODO Change this so it ignores things already used *)

    method private take_within (pos : int * int) : string list =
      let inside, outside =
        List.partition
          (fun (_, start_pos, end_pos) ->
            let start_range, end_range = pos in
            end_pos <= end_range)
          comments
      in
      comments <- outside;
      List.map (fun (s, _, _) -> s) inside

    method private string_to_tag (s : string) : atag = s

    method private string_to_cite (s : string) : cite =
      let pay_str : Ppxlib.Parsetree.structure_item =
        Builder.pstr_eval
          (Builder.pexp_constant
             (Ppxlib.Parsetree.Pconst_string (s, Location.none, None)))
          []
      in

      PStr [ pay_str ]

    method private create_attr (name : string) (payload : string) : attr =
      let attr_name : string Location.loc = Location.mknoloc name in
      let attr_payload = self#string_to_cite payload in
      Builder.attribute ~name:attr_name ~payload:attr_payload

    val comment_regex : Re.re =
      Re.compile
        (Re.seq [ Re.str "(*"; Re.shortest (Re.rep Re.any); Re.str "*)" ])

    method private retrieve_comments (start_pos : Lexing.position)
        (end_pos : Lexing.position) : string list =
      try
        let body_text = self#retrieve_comments_text start_pos end_pos in
        body_text
      with
      | CommentNotFound -> []
      | e -> raise e

    method private retrieve_comments_text (start_pos : Lexing.position)
        (end_pos : Lexing.position) : string list =
      try
        assert (end_pos.pos_cnum >= start_pos.pos_cnum);
        assert (start_pos.pos_cnum >= 0);

        let body_text =
          self#take_within (start_pos.pos_cnum, end_pos.pos_cnum)
        in

        body_text
      with
      | Stdlib.Invalid_argument _ -> raise CommentNotFound
      | e -> raise e

    method private comment_str (s : string) : Ppxlib.Parsetree.structure =
      let attr_specification : Ppxlib.Parsetree.structure_item =
        Builder.pstr_eval
          (Builder.pexp_constant
             (Ppxlib.Parsetree.Pconst_string
                (s, Location.none, None (* FIXME ?*))))
          []
      in
      [ attr_specification ]

    method private comment_attr (b : string) : attr =
      let attr_name : string Location.loc = Location.mknoloc "sml.comment" in
      let attr_payload = self#string_to_cite b in
      Builder.attribute ~name:attr_name ~payload:attr_payload

    method cite
        : 'a. 'a citer -> (Lexing.position * Lexing.position) option -> 'a -> 'a
        =
      fun tag pos x ->
        match pos with
        | None -> x
        | Some (start_pos, end_pos) ->
            let comments = self#retrieve_comments start_pos end_pos in
            let comments' = List.map self#comment_attr comments in
            List.fold_left tag x comments'

    method cite_exact : 'a. 'a citer -> string -> string list -> 'a -> 'a =
      fun tag name payload x ->
        let payload_str = payload in
        let attrs = List.map (self#create_attr name) payload_str in
        List.fold_left tag x attrs

    method destruct : unit -> bool =
      fun () ->
        let res = comments == [] in
        res
  end
