include Helpers

type atag = string
type attr = Parsetree.attribute
type cite = Parsetree.payload
type 'a citer = 'a -> attr -> 'a

exception CommentNotFound

let fixed_name (old_name : string) (new_name : string) : attr =
  let attr_name : string Location.loc = Location.mknoloc "sml.fixed_name" in
  let payload_str : Ppxlib.Parsetree.structure_item =
    Builder.pstr_eval
      (Builder.pexp_constant
         (Ppxlib.Parsetree.Pconst_string (new_name, Location.none, None)))
      []
  in
  let attr_payload : Ppxlib.Parsetree.payload = PStr [ payload_str ] in
  Builder.attribute ~name:attr_name ~payload:attr_payload

let change_name (old_name : string) (new_name : string) : attr =
  let attr_name : string Location.loc = Location.mknoloc "sml.change_name" in
  let payload_str : Ppxlib.Parsetree.structure_item =
    Builder.pstr_eval
      (Builder.pexp_constant
         (Ppxlib.Parsetree.Pconst_string (new_name, Location.none, None)))
      []
  in
  let attr_payload : Ppxlib.Parsetree.payload = PStr [ payload_str ] in
  Builder.attribute ~name:attr_name ~payload:attr_payload

let rec get_all_comments (lexbuf : string) : (string * int * int) list =
  let rec comment_regex' depth =
    let inner_re =
      if depth > 0 then
        Re.first
          (Re.alt [ comment_regex' (depth - 1); Re.shortest (Re.rep Re.any) ])
      else Re.shortest (Re.rep Re.any)
    in
    Re.seq [ Re.str "(*"; inner_re; Re.str "*)" ]
  in
  let comment_regex = Re.compile @@ comment_regex' 10 in
  let comments = Re.all comment_regex lexbuf in
  let process_comment_group : Re.Group.t -> string * int * int =
   fun group ->
    let comment_str = Re.Group.get group 0 in
    let start_pos = Re.Group.start group 0 in
    let end_pos = Re.Group.stop group 0 in
    (comment_str, start_pos, end_pos)
  in
  List.map process_comment_group comments

module StringSet = Set.Make(String)

class process_label opts lexbuf =
  object (self)
    val config : Common.t = opts
    val mutable comments : (string * int * int) list = get_all_comments lexbuf

    (** Read-only copy of all comments for position lookups *)
    val all_original_comments : (string * int * int) list = get_all_comments lexbuf

    (** Set of comment texts already emitted, keyed by "text@position" to avoid
        deduplicating genuinely repeated comments at different positions *)
    val mutable emitted_comment_keys : StringSet.t = StringSet.empty

    val mutable lexbuf : string =
      lexbuf (* TODO Change this so it ignores things already used *)

    (* Comment tracking for error detection *)
    val mutable total_comments_seen : int = 0
    val mutable total_comments_emitted : int = 0

    (* NEW FIELDS for comment hoisting *)
    val mutable pending_comments : (string * int) list = []
    (* Comments accumulated during expression/pattern processing *)
    (* Tuple: (comment_text, original_char_position) *)

    val mutable last_structure_pos : int = 0
    (* Position of last structure boundary, for flushing *)

    val mutable emission_mode : [ `Immediate | `Accumulate ] = `Immediate
    (* Controls whether comments are attached or accumulated *)

    method private take_within (pos : int * int) : string list =
      let start_range, end_range = pos in
      (* Find comments that are strictly within the given range *)
      let inside, outside =
        List.partition
          (fun (_, comment_start, comment_end) ->
            comment_start >= start_range && comment_end <= end_range)
          comments
      in
      comments <- outside;
      (* Sort by start position to maintain source order *)
      let sorted_inside =
        List.sort (fun (_, s1, _) (_, s2, _) -> compare s1 s2) inside
      in
      List.map (fun (s, _, _) -> s) sorted_inside

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

    method private lookup_comment_position (text : string) : int =
      match List.find_opt (fun (t, _, _) -> t = text) all_original_comments with
      | Some (_, start, _) -> start
      | None -> 0

    (** Make a dedup key for a comment at a known position *)
    method private make_dedup_key (text : string) (pos : int) : string =
      text ^ "@" ^ string_of_int pos

    (** Check if a comment text has already been emitted (by any path).
        Uses position-based matching against the original comment list. *)
    method private is_already_emitted (text : string) : bool =
      (* Check if any instance of this text has been marked as emitted *)
      List.exists (fun (t, start, _) ->
        t = text && StringSet.mem (self#make_dedup_key t start) emitted_comment_keys
      ) all_original_comments

    (** Mark a comment at a specific position as emitted *)
    method private mark_emitted_at_pos (text : string) (pos : int) : unit =
      emitted_comment_keys <- StringSet.add (self#make_dedup_key text pos) emitted_comment_keys

    (** Mark a comment by text as emitted (finds first unEmitted position) *)
    method private mark_emitted (text : string) : unit =
      match List.find_opt (fun (t, start, _) ->
        t = text && not (StringSet.mem (self#make_dedup_key t start) emitted_comment_keys)
      ) all_original_comments with
      | Some (_, start, _) ->
          emitted_comment_keys <- StringSet.add (self#make_dedup_key text start) emitted_comment_keys
      | None -> ()

    method cite :
        'a. 'a citer -> string list -> 'a -> 'a =
      fun tag node_comments x ->
        total_comments_seen <- total_comments_seen + List.length node_comments;
        (* Count all as emitted (deduped ones were already emitted by leading_comments) *)
        total_comments_emitted <- total_comments_emitted + List.length node_comments;
        (* Filter out comments already emitted by leading_comments *)
        let fresh_comments = List.filter (fun c -> not (self#is_already_emitted c)) node_comments in
        match fresh_comments with
        | [] -> x
        | _ -> (
            match emission_mode with
            | `Immediate ->
                (* Attach as attributes directly *)
                let comments' = List.map (fun c ->
                  self#mark_emitted c;
                  self#comment_attr c) fresh_comments in
                List.fold_left tag x comments'
            | `Accumulate ->
                (* Accumulate for later emission with actual positions *)
                let with_positions =
                  List.map (fun c ->
                    self#mark_emitted c;
                    (c, self#lookup_comment_position c)) fresh_comments
                in
                pending_comments <- pending_comments @ with_positions;
                x (* Return unchanged *))

    method until : Lexing.position -> Attr.attr list =
      fun end_pos ->
        (* Get comments from position 0 up to end_pos *)
        let start_pos = { Lexing.dummy_pos with pos_cnum = 0 } in
        let comments = self#retrieve_comments start_pos end_pos in
        List.map self#comment_attr comments

    method cite_exact : 'a. 'a citer -> string -> string list -> 'a -> 'a =
      fun tag name payload x ->
        let payload_str = payload in
        let attrs = List.map (self#create_attr name) payload_str in
        List.fold_left tag x attrs

    (* NEW METHODS for comment hoisting *)

    method enter_accumulate_context : unit = emission_mode <- `Accumulate
    (** Enter accumulation mode for expression/pattern context *)

    method exit_accumulate_context : unit = emission_mode <- `Immediate
    (** Exit accumulation mode, return to immediate *)

    method flush_pending : (string * int) list =
      let matching, remaining =
        List.partition
          (fun (_, pos) -> pos > last_structure_pos)
          pending_comments
      in
      pending_comments <- remaining;
      List.sort (fun (_, p1) (_, p2) -> compare p1 p2) matching
    (** Get pending comments after last structure position, sorted by position
    *)

    method mark_structure_boundary (pos : int) : unit =
      last_structure_pos <- pos
    (** Update structure boundary marker *)

    (** Save last_structure_pos and set it to a new value.
        Returns the old value to restore later via [restore_structure_boundary].
        Use when entering a nested scope (struct/sig body). *)
    method push_structure_boundary (pos : int) : int =
      let saved = last_structure_pos in
      last_structure_pos <- pos;
      saved

    (** Restore last_structure_pos to a previously saved value.
        Use when leaving a nested scope. *)
    method restore_structure_boundary (saved : int) : unit =
      last_structure_pos <- saved

    method leading_comments (pos : Lexing.position option) :
        Parsetree.structure_item list =
      match pos with
      | None -> []
      | Some start_pos ->
          let start_range = last_structure_pos in
          let end_range = start_pos.Lexing.pos_cnum in
          if end_range <= start_range then []
          else begin
            let leading = self#take_within (start_range, end_range) in
            last_structure_pos <- end_range;
            List.map
              (fun comment_text ->
                self#mark_emitted comment_text;
                let attr = self#comment_attr comment_text in
                {
                  Parsetree.pstr_desc = Parsetree.Pstr_attribute attr;
                  Parsetree.pstr_loc = Location.none;
                })
              leading
          end
    (** Get leading comments before a position (from last_structure_pos to pos)
        and update last_structure_pos to the end of this range *)

    method leading_signature_comments (pos : Lexing.position option) :
        Parsetree.signature_item list =
      match pos with
      | None -> []
      | Some start_pos ->
          let start_range = last_structure_pos in
          let end_range = start_pos.Lexing.pos_cnum in
          if end_range <= start_range then []
          else begin
            let leading = self#take_within (start_range, end_range) in
            last_structure_pos <- end_range;
            List.map
              (fun comment_text ->
                self#mark_emitted comment_text;
                let attr = self#comment_attr comment_text in
                {
                  Parsetree.psig_desc = Parsetree.Psig_attribute attr;
                  Parsetree.psig_loc = Location.none;
                })
              leading
          end
    (** Get leading comments for signatures *)

    method emit_pending_as_structure_items :
        unit -> Parsetree.structure_item list =
      fun () ->
        let pending = self#flush_pending in
        List.map
          (fun (comment_text, _) ->
            let attr = self#comment_attr comment_text in
            {
              Parsetree.pstr_desc = Parsetree.Pstr_attribute attr;
              Parsetree.pstr_loc = Location.none;
            })
          pending
    (** Create Pstr_attribute items from pending comments *)

    method emit_pending_as_signature_items :
        unit -> Parsetree.signature_item list =
      fun () ->
        let pending = self#flush_pending in
        List.map
          (fun (comment_text, _) ->
            let attr = self#comment_attr comment_text in
            {
              Parsetree.psig_desc = Parsetree.Psig_attribute attr;
              Parsetree.psig_loc = Location.none;
            })
          pending

    (** Extract comments from the regex pool that fall within the given AST node
        position range, and attach them as attributes on the node using the citer.
        This is the primary mechanism for placing comments on expression/pattern nodes
        whose .comments field may be empty (because frontend distribution is coarse). *)
    method cite_for_node :
        'a. 'a citer ->
        (Lexing.position * Lexing.position) option ->
        'a -> 'a =
      fun tag pos x ->
        match pos with
        | None -> x
        | Some (start_pos, end_pos) ->
            let within = self#take_within (start_pos.pos_cnum, end_pos.pos_cnum) in
            match within with
            | [] -> x
            | _ ->
                total_comments_seen <- total_comments_seen + List.length within;
                total_comments_emitted <- total_comments_emitted + List.length within;
                let attrs = List.map (fun c ->
                  self#mark_emitted c;
                  self#comment_attr c) within in
                List.fold_left tag x attrs

    method flush_all_remaining_as_structure_items : Parsetree.structure_item list =
      (* Flush any pending accumulated comments *)
      let pending = self#emit_pending_as_structure_items () in
      (* Flush any remaining comments from the regex pool, skipping already-emitted *)
      let remaining = comments in
      comments <- [];
      let fresh_remaining = List.filter
        (fun (text, _, _) -> not (self#is_already_emitted text))
        remaining in
      let remaining_items = List.map
        (fun (comment_text, _, _) ->
          self#mark_emitted comment_text;
          let attr = self#comment_attr comment_text in
          { Parsetree.pstr_desc = Parsetree.Pstr_attribute attr;
            Parsetree.pstr_loc = Location.none })
        fresh_remaining in
      pending @ remaining_items

    method destruct : unit -> bool =
      fun () ->
        let res = comments == [] in
        res

    method comments_to_structure_items (node_comments : string list) : Parsetree.structure_item list =
      total_comments_seen <- total_comments_seen + List.length node_comments;
      (* Count all as emitted (deduped ones were already emitted by leading_comments) *)
      total_comments_emitted <- total_comments_emitted + List.length node_comments;
      (* Filter out comments already emitted by leading_comments *)
      let fresh_comments = List.filter (fun c -> not (self#is_already_emitted c)) node_comments in
      List.map (fun comment_text ->
        self#mark_emitted comment_text;
        let attr = self#comment_attr comment_text in
        { Parsetree.pstr_desc = Parsetree.Pstr_attribute attr;
          Parsetree.pstr_loc = Location.none }) fresh_comments

    method check_all_comments_emitted : unit =
      if total_comments_seen > 0 && total_comments_emitted < total_comments_seen then
        failwith
          (Printf.sprintf
             "Comment conversion error: %d comments were seen but only %d were emitted to the output OCaml. %d comment(s) were dropped."
             total_comments_seen total_comments_emitted
             (total_comments_seen - total_comments_emitted))
  end
