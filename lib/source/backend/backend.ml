(** {1 SML to OCaml Backend}

    This module implements the backend of the SML to OCaml converter,
    transforming the SML abstract syntax tree into OCaml's Parsetree
    representation for code generation.

    The conversion follows these key principles:
    - SML types map to OCaml types with adjustments for syntax differences
    - SML module system (structures, signatures, functors) maps to OCaml modules
    - Pattern matching and expressions are converted with equivalent semantics
    - Name processing handles reserved word conflicts and capitalization
      conventions

    @see <https://ocaml.org/api/compilerlibref/Parsetree.html>
      OCaml Parsetree documentation
    @see <http://sml-family.org/sml97-defn.pdf> Standard ML definition *)

open Backend_sig

(** Result type for the complete conversion. Currently unspecified; will contain
    the final OCaml structure/signature. *)

include Helpers
open Common
module Debug = Ppxlib.Pprintast
module ContextLib = Context  (* Library Context module before shadowing *)

(* Re-export helper modules for use within the functor *)
module Idx_utils = Idx_utils
module Type_var_utils = Type_var_utils
module Capital_utils = Capital_utils

module Make (Context : CONTEXT) (Config : CONFIG) = struct
  let config = Config.config
  let quoter = Ppxlib.Expansion_helpers.Quoter.create ()
  let labeller = new Process_label.process_label config Context.lexbuf
  let lexbuf = Context.lexbuf
  let current_temp : int ref = ref 0 
  let get_current_then (i : int) : int =
    let res = !current_temp in
    current_temp := !current_temp + i;
    res
  module Log = Common.Make (struct
    let config = Config.config
    let group = "backend"
  end)

  (* Name processing removed - using literal translation only *)
  let get_signature_attr pos = 
    let attrs = labeller#until pos in 
    List.map (fun attr -> 
      { Parsetree.psig_desc = Parsetree.Psig_attribute attr; 
        Parsetree.psig_loc = Location.none }) attrs
  let get_structure_attr pos = 
    let attrs = labeller#until pos in 
    List.map (fun attr -> 
      { Parsetree.pstr_desc = Parsetree.Pstr_attribute attr;
        Parsetree.pstr_loc = Location.none }) attrs
  (* Constructor tracking removed - using literal translation only *)

  (* Name processing removed - using literal translation only *)

  let sanitize_ident (s : string) : string =
    let buf = Buffer.create (String.length s) in
    String.iter
      (fun c ->
        match c with
        | '\'' -> Buffer.add_string buf "_prime"
        | '`' -> Buffer.add_string buf "_bq"
        | _ -> Buffer.add_char buf c)
      s;
    Buffer.contents buf

  (** Build a Longident from name parts (sanitized to valid OCaml identifiers) *)
  let build_longident (parts : string list) : Ppxlib.Longident.t =
    match parts |> List.map sanitize_ident with
    | [] -> failwith "empty name"
    | [ x ] -> Longident.Lident x
    | first :: rest ->
        List.fold_left
          (fun acc part -> Longident.Ldot (acc, part))
          (Longident.Lident first) rest

  (** Get string from name parts (sanitize to valid OCaml identifier) *)
  let name_to_string (parts : string list) : string =
    match parts |> List.map sanitize_ident with
    | [] -> failwith "empty name"
    | parts -> List.nth parts (List.length parts - 1)

  type res = Parsetree.toplevel_phrase list

  module Config = Config

  let current_path : string list ref = ref []

  let push_module name =
    current_path := !current_path @ [name]

  let pop_module () =
    match List.rev !current_path with
    | _ :: rest -> current_path := List.rev rest
    | [] -> ()

  let register_constructor name =
    let ocaml_name = Constructor_transform.transform_constructor name in
    let full_path = !current_path @ [name] in
    Constructor_registry.add_constructor
      Context.context.constructor_registry
      ~path:full_path
      ~name
      ~ocaml_name

  exception BadAst of (Lexing.position * Lexing.position) option * string

  let mkBadAst ?loc (msg : string) : exn = BadAst (loc, msg)
  let mkLoc (v : 'a) (loc : Location.t) : 'a Location.loc = { txt = v; loc }

  (** Helper function to create a located value with no source location.

      @param v The value to wrap with a phantom location
      @return The value wrapped in a {!Location.loc} structure *)
  let ghost (v : 'a) : 'a Location.loc = mkLoc v Location.none

  let depth : int ref = ref 0

  let trace_part ?(level = 2) ?(ast = "") ?(msg = "") ~value : 'a =
    match get_verbosity config with
    | Some v when v >= level ->
        (* TODO use level *)
        let indent = !depth in
        depth := indent + 1;
        Log.log ~subgroup:"trace" ~level:Debug ~kind:Neutral
          ~msg:(Stdlib.Format.sprintf "%dEntering %s %s" !depth ast msg)
          ();
        let res = value () in
        depth := indent;
        Log.log ~subgroup:"trace" ~level:Debug ~kind:Neutral
          ~msg:(Stdlib.Format.sprintf "%dExiting %s %s" !depth ast msg)
          ();
        res
    | _ -> value ()

  (* Use helper modules for common operations *)
  let is_variable_identifier = Capital_utils.is_variable_identifier
  let idx_to_string = Idx_utils.idx_to_string
  let process_lowercase = Capital_utils.process_lowercase
  let process_uppercase = Capital_utils.process_uppercase
  
  let idx_to_name (idx : Ast.idx) : string list =
    Idx_utils.idx_to_name idx

  (** Main entry point for converting a complete SML program.

      @param prog The SML program to convert
      @return The converted OCaml representation as toplevel phrases

      This is implemented after {!process_prog} in the mutually recursive chain.
  *)

  (** {2 Type Processing}

      Functions for converting SML types to OCaml types.

      SML and OCaml have similar type systems, but with syntactic differences:
      - SML: [int * string -> bool] vs OCaml: [int * string -> bool]
      - SML: ['a list] vs OCaml: ['a list]
      - SML: [{x: int, y: int}] (records) vs OCaml: [< x: int; y: int >]
        (objects)

      The main difference is that SML records are converted to OCaml objects. *)

  (* Instantiate type processor module with dependencies *)
  module TypeDeps = struct
    let labeller = labeller
    let build_longident = build_longident
    let name_to_string = name_to_string
    let ghost = ghost
    let config = config
  end
  module Types = Backend_types.Make(TypeDeps)

  (** Convert an SML type to an OCaml core type. Delegated to Backend_types. *)
  let process_type_value = Types.process_type_value

  (** Convert SML record type rows to OCaml object fields. Delegated to Backend_types. *)
  let process_object_field_type = Types.process_object_field_type

  (** Wrapper function for {!process_type_value}.

      @param ty The SML type to convert
      @return The corresponding OCaml core type *)
  let process_type (ty : Ast.typ node) : Parsetree.core_type =
    Log.log ~subgroup:"type" ~level:Debug ~kind:Neutral
      ~msg:"Processing type conversion"
      ();
    trace_part ~level:2 ~ast:"typ" ~msg:"" (* ~msg:(Ast.show_typ ty) *)
      ~value:(fun () -> process_type_value ty)

  (** {2 Constant Processing}

      Functions for converting SML constants to OCaml constants.

      Note: SML and OCaml have different constant syntaxes that need
      translation:
      - SML uses [~] for negation, OCaml uses [-]
      - SML has word literals ([0w42]), OCaml doesn't (need conversion)
      - SML character literals use [#"c"], OCaml uses ['c'] *)

  (** Convert an SML constant to an OCaml constant. Delegated to Backend_constants. *)
  (* TODO Task 1.4: Wire Backend_const here *)
  (* Original process_con function to be replaced *)
  let process_con = Backend_constants.process_con

  let rec is_operator (s : expression Ast.node) : bool =
    match s.value with
    | ExpIdx idx ->
        let name = idx_to_name idx.value in
        is_operator_name (List.hd name)
    | ParenExp e -> is_operator e
    | _ -> false

  and is_operator_name = Capital_utils.is_operator_name

  (** {2 Expression Processing}

      Functions for converting SML expressions to OCaml expressions.

      Expression conversion is mostly straightforward, with key differences:
      - SML's [andalso]/[orelse] → OCaml's [&&]/[||]
      - SML's [fn] → OCaml's [fun]
      - SML's sequential expressions [(e1; e2; e3)] → OCaml's [e1; e2; e3]
      - Let expressions require different structuring *)

  (** Convert an SML expression to an OCaml expression.

    Currently handles:
    - Constants (via {!process_con})
    - Identifiers (with proper name processing)
    - Function application ([f x])
    - Infix operators ([x + y])

    @param expression The SML expression to convert
    @return The corresponding OCaml {!Parsetree.expression}
    @raise Assert_failure For unimplemented expression forms

    @example
    {[
      (* SML: f x *)
      process_exp (ExpApp (ExpIdx "f", ExpIdx "x"))
      (* → OCaml Parsetree for: f x *)

      (* SML: x + y *)
      process_exp (InfixApp (ExpIdx "x", "+", ExpIdx "y"))
      (* → OCaml Parsetree for: (+) x y *)
    ]} *)

  let rec process_exp (expression : Ast.expression Ast.node) :
      Parsetree.expression =
    Log.log ~subgroup:"expression" ~level:Debug ~kind:Neutral
      ~msg:(Printf.sprintf "Processing expression: %s"
        (match expression.value with
         | ExpCon _ -> "ExpCon"
         | ExpApp _ -> "ExpApp"
         | ExpIdx _ -> "ExpIdx"
         | InfixApp _ -> "InfixApp"
         | ParenExp _ -> "ParenExp"
         | TupleExp _ -> "TupleExp"
         | RecordExp _ -> "RecordExp"
         | RecordSelector _ -> "RecordSelector"
         | ArrayExp _ -> "ArrayExp"
         | ListExp _ -> "ListExp"
         | SeqExp _ -> "SeqExp"
         | LetExp _ -> "LetExp"
         | TypedExp _ -> "TypedExp"
         | RaiseExp _ -> "RaiseExp"
         | HandleExp _ -> "HandleExp"
         | AndExp _ -> "AndExp"
         | OrExp _ -> "OrExp"
         | IfExp _ -> "IfExp"
         | WhileExp _ -> "WhileExp"
         | CaseExp _ -> "CaseExp"
         | FnExp _ -> "FnExp"))
      ();
    let res = begin match expression.value with
    | ExpCon c -> Builder.pexp_constant (process_con c)
    | ExpApp (e1, e2) when is_operator e2 ->
        let op_name =
          match e2.value with
          | ExpIdx idx -> idx_to_name idx.value
          | ParenExp e -> (
              match e.value with
              | ExpIdx idx -> idx_to_name idx.value
              | _ -> failwith "Expected operator identifier")
          | _ -> failwith "Expected operator identifier"
        in
        let op_longident = build_longident op_name in
        Builder.pexp_apply
          (Builder.pexp_ident (ghost op_longident))
          [ (Nolabel, process_exp e1) ]
    | ExpApp (e1, e2) -> (
        (* Check if this is a constructor applied to a tuple *)
        match (e1.value, e2.value) with
        | (ExpIdx idx_node, TupleExp args) -> (
            let idx_name = idx_to_name idx_node.value in
            let idx_str = idx_to_string idx_node.value in
            (* Look up constructor in registry *)
            let lookup_result = Constructor_registry.lookup
              Context.context.constructor_registry ~path:None idx_str in
            (match lookup_result with
            | Some ctor_info ->
                (* It's a constructor - use transformed name *)
                let name_longident = build_longident [ctor_info.Constructor_registry.ocaml_name] in
                let arg_tuple = Builder.pexp_tuple (List.map process_exp args) in
                Builder.pexp_construct (ghost name_longident) (Some arg_tuple)
            | None ->
                (* Variable applied to tuple - use regular application *)
                let e1' = process_exp e1 in
                let e2' = Builder.pexp_tuple (List.map process_exp args) in
                Builder.pexp_apply e1' [ (Nolabel, e2') ])
          )
        | _ ->
            (* Regular application *)
            let e1' = process_exp e1 in
            let e2' = process_exp e2 in
            Builder.pexp_apply e1' [ (Nolabel, e2') ]
      )
    | ExpIdx idx ->
        let scoped_name = idx_to_name idx.value in
        let simple_name = name_to_string scoped_name in
        (* Extract module path if qualified *)
        let qual_path =
          if List.length scoped_name > 1 then
            Some (List.rev (List.tl (List.rev scoped_name)))
          else None
        in
        (* Try to look up as constructor *)
        let lookup_result = Constructor_registry.lookup
          Context.context.constructor_registry ~path:qual_path simple_name in
        (match lookup_result with
        | Some ctor_info ->
            (* Constructor reference - use transformed name *)
            let transformed_parts = match qual_path with
              | Some path -> path @ [ctor_info.Constructor_registry.ocaml_name]
              | None -> [ctor_info.ocaml_name]
            in
            let name_longident = build_longident transformed_parts in
            Builder.pexp_construct (ghost name_longident) None
        | None ->
            (* Value reference - use original logic *)
            let name_longident = build_longident scoped_name in
            Builder.pexp_ident (ghost name_longident))
    | InfixApp (e1, op, e2) ->
        let op_name = idx_to_string op.value in
        (* Look up constructor in registry for infix operators like :: *)
        let lookup_result = Constructor_registry.lookup
          Context.context.constructor_registry ~path:None op_name in
        (match lookup_result with
        | Some ctor_info ->
            (* Infix constructor - use pexp_construct *)
            let name_longident = build_longident [ctor_info.Constructor_registry.ocaml_name] in
            let tuple = Builder.pexp_tuple [process_exp e1; process_exp e2] in
            Builder.pexp_construct (ghost name_longident) (Some tuple)
        | None ->
            (* Regular infix operator *)
            let op_longident = build_longident (idx_to_name op.value) in
            Builder.pexp_apply
              (Builder.pexp_ident (ghost op_longident))
              [ (Nolabel, process_exp e1); (Nolabel, process_exp e2) ])
    | ParenExp e -> process_exp e
    | TupleExp [] ->
        Builder.pexp_construct (ghost (Ppxlib.Longident.Lident "()")) None
    | TupleExp exps ->
        Builder.pexp_tuple (List.map (fun e -> process_exp e) exps)
    | RecordExp rows ->
        let fields =
          List.map
            (fun r ->
              match r.Ast.value with
              | Ast.Row (lab, expression, _) ->
                  let lab_longident =
                    build_longident (idx_to_name lab.value)
                  in
                  (ghost lab_longident, process_exp expression))
            rows
        in
        Builder.pexp_record fields None
    | RecordSelector lab ->
        (* #label -> fun r -> r.label *)
        let lab_str =
          name_to_string (idx_to_name lab.value)
        in
        let r_pat = Builder.ppat_var (ghost "r") in
        let r_exp = Builder.pexp_ident (ghost (Ppxlib.Longident.Lident "r")) in
        let field_exp =
          Builder.pexp_field r_exp (ghost (Ppxlib.Longident.Lident lab_str))
        in
        Builder.pexp_fun Nolabel None r_pat field_exp
    | ArrayExp exps -> Builder.pexp_array (List.map process_exp exps) 
    | ListExp exps ->
        (* Build list from right to left using :: *)
        List.fold_right
          (fun e acc ->
            Builder.pexp_construct
              (ghost (Ppxlib.Longident.Lident "::"))
              (Some (Builder.pexp_tuple [ process_exp e; acc ])))
          exps
          (Builder.pexp_construct (ghost (Ppxlib.Longident.Lident "[]")) None)
    | SeqExp exps ->
        (* Build sequence expression from list *)
        let rec build_seq = function
          | [] ->
              Builder.pexp_construct (ghost (Ppxlib.Longident.Lident "()")) None
          | [ e ] -> process_exp e
          | e :: rest -> Builder.pexp_sequence (process_exp e) (build_seq rest)
        in
        build_seq exps
    | LetExp ([], exps) ->
        process_exp { value = SeqExp exps; pos = expression.pos }
    | LetExp (dec :: decs, exps) -> (
        
        (* First, flatten SeqDec to handle each declaration individually *)
        let flattened_decs =
          let rec flatten_dec d =
            match d.value with
            | SeqDec inner_decs -> List.concat (List.map flatten_dec inner_decs)
            | _ -> [ d ]
          in
          flatten_dec dec
        in
        (* Process flattened declarations *)
        let all_decs = flattened_decs @ decs in
        match all_decs with
        | [] -> process_exp { value = SeqExp exps; pos = expression.pos }
        | first_dec :: rest_decs -> (
            Log.log ~subgroup:"let-expr" ~level:Debug ~kind:Neutral
              ~msg:(Printf.sprintf "Processing LetExp with declaration: %s"
                (match first_dec.value with
                 | ExnDec _ -> "ExnDec"
                 | DatDec _ -> "DatDec"
                 | TypDec _ -> "TypDec"
                 | LocalDec _ -> "LocalDec"
                 | OpenDec _ -> "OpenDec"
                 | FixityDec _ -> "FixityDec"
                 | DataDecAlias _ -> "DataDecAlias"
                 | AbstractDec _ -> "AbstractDec"
                 | StrDec _ -> "StrDec"
                 | SeqDec _ -> "SeqDec"
                 | ValDec _ -> "ValDec"
                 | FunDec _ -> "FunDec"
                 | ExpDec _ -> "ExpDec"))
              ();
            match first_dec.value with
            | ExnDec eb ->
                (* Handle exception declarations in let expressions *)
                (* SML: let exception E of t in ... end *)
                (* OCaml: let exception E of t in ... *)
                let exn_constrs = process_exn_bind eb.value in
                let body =
                  process_exp
                    { value = LetExp (rest_decs, exps); pos = expression.pos }
                  |> labeller#cite Helpers.Attr.expression expression.pos
                in
                (* Build nested let exception expressions *)
                List.fold_right
                  (fun ext_constr acc ->
                    Builder.pexp_letexception ext_constr acc)
                  exn_constrs body
            | DatDec (db, tb_opt) ->
                (* Handle datatype declarations in let expressions *)
                (* SML: let datatype t = A | B in ... end *)
                (* OCaml: let module M = struct type t = A | B end in ... *)
                let tdecls = process_dat_bind db.value in
                let type_items =
                  match tb_opt with
                  | None -> [ Builder.pstr_type Asttypes.Recursive tdecls ]
                  | Some tb ->
                      let tb_decls = process_typ_bind tb.value in
                      [
                        Builder.pstr_type Asttypes.Recursive tdecls;
                        Builder.pstr_type Asttypes.Recursive tb_decls;
                      ]
                in
                let mod_name = ghost (Some "_Types") in
                let mod_expr = Builder.pmod_structure type_items in
                let body =
                  process_exp
                    { value = LetExp (rest_decs, exps); pos = expression.pos }
                  |> labeller#cite Helpers.Attr.expression expression.pos
                in
                Builder.pexp_letmodule mod_name mod_expr body
            | TypDec tb ->
                (* Handle type declarations in let expressions *)
                (* SML: let type t = int in ... end *)
                (* OCaml: let module M = struct type t = int end in ... *)
                let tdecls = process_typ_bind tb.value in
                let type_items =
                  [ Builder.pstr_type Asttypes.Nonrecursive tdecls ]
                in
                let mod_name = ghost (Some "_Types") in
                let mod_expr = Builder.pmod_structure type_items in
                let body =
                  process_exp
                    { value = LetExp (rest_decs, exps); pos = expression.pos }
                  |> labeller#cite Helpers.Attr.expression expression.pos
                in
                Builder.pexp_letmodule mod_name mod_expr body
            | LocalDec (d1, d2) ->
                (* Handle local declarations in let expressions *)
                (* SML: let local dec1 in dec2 end in ... end *)
                (* OCaml: let <bindings from dec1 and dec2> in ... *)
                (* Process as nested let: first dec1, then dec2, then rest *)
                process_exp
                  {
                    value = LetExp ([ d1; d2 ] @ rest_decs, exps);
                    pos = expression.pos;
                  }
            | OpenDec ids ->
                (* Handle open declarations in let expressions *)
                (* SML: let open M in ... end *)
                (* OCaml: let open M in ... *)
                let body =
                  process_exp
                    { value = LetExp (rest_decs, exps); pos = expression.pos }
                  |> labeller#cite Helpers.Attr.expression expression.pos
                in
                (* Process open declarations from right to left to maintain proper scoping *)
                List.fold_right
                  (fun (id : Ast.idx Ast.node) acc ->
                    let longid =
                      build_longident (idx_to_name id.value)
                    in
                    let mod_expr = Builder.pmod_ident (ghost longid) in
                    let open_infos =
                      Builder.open_infos ~override:Asttypes.Fresh ~expr:mod_expr
                    in
                    Builder.pexp_open open_infos acc)
                  ids body
            | FixityDec _ ->
                (* Fixity declarations have no runtime effect - skip and process rest *)
                process_exp
                  { value = LetExp (rest_decs, exps); pos = expression.pos }
            | DataDecAlias (id1, id2) ->
                (* Datatype alias in let expression *)
                (* SML: let datatype t = datatype u in ... end *)
                (* OCaml: let module M = struct type t = u end in ... *)
                let name1_str =
                  name_to_string (idx_to_name id1.value)
                in
                let longid2 =
                  build_longident (idx_to_name id2.value)
                in
                let alias_type = Builder.ptyp_constr (ghost longid2) [] in
                let tdecl =
                  labeller#cite Helpers.Attr.type_declaration id1.pos
                    (Builder.type_declaration ~name:(ghost name1_str) ~params:[]
                       ~cstrs:[] ~kind:Parsetree.Ptype_abstract
                       ~private_:Asttypes.Public ~manifest:(Some alias_type))
                in
                let type_items =
                  [ Builder.pstr_type Asttypes.Recursive [ tdecl ] ]
                in
                let mod_name = ghost (Some "_Types") in
                let mod_expr = Builder.pmod_structure type_items in
                let body =
                  process_exp
                    { value = LetExp (rest_decs, exps); pos = expression.pos }
                  |> labeller#cite Helpers.Attr.expression expression.pos
                in
                Builder.pexp_letmodule mod_name mod_expr body
            | AbstractDec (db, tb_opt, inner_decs) ->
                (* Abstract type in let expression *)
                (* Process the inner declarations, hiding the datatype constructors *)
                process_exp
                  {
                    value = LetExp (inner_decs @ rest_decs, exps);
                    pos = expression.pos;
                  }
            | StrDec _ ->
                (* Structure declarations are not allowed in let expressions per SML spec *)
                raise
                  (BadAst
                     ( first_dec.pos,
                       "Structure declaration not allowed in let expression" ))
            | SeqDec inner_decs ->
                (* Should have been flattened, but handle it just in case *)
                process_exp
                  {
                    value = LetExp (inner_decs @ rest_decs, exps);
                    pos = expression.pos;
                  }
            | ValDec _ ->
                (* Handle value declarations in let expressions *)
                let binding = process_value_dec first_dec.value in
                let body =
                  process_exp
                    { value = LetExp (rest_decs, exps); pos = expression.pos }
                  |> labeller#cite Helpers.Attr.expression expression.pos
                in
                Builder.pexp_let Nonrecursive binding body
            | FunDec _ ->
                (* Handle value and function declarations *)
                let binding = process_value_dec first_dec.value in
                let body =
                  process_exp
                    { value = LetExp (rest_decs, exps); pos = expression.pos }
                  |> labeller#cite Helpers.Attr.expression expression.pos
                in
                Builder.pexp_let Recursive binding body)
        | _ -> assert false)
    | TypedExp (e, ty) ->
        Builder.pexp_constraint (process_exp e) (process_type ty)
    | RaiseExp e ->
        let e' = process_exp e in
        Builder.pexp_apply
          (Builder.pexp_ident (ghost (Ppxlib.Longident.Lident "raise")))
          [ (Nolabel, e') ]
    | HandleExp (e, cases) ->
        Builder.pexp_try (process_exp e) (process_matching cases.value)
    | AndExp (e1, e2) ->
        (* andalso -> && *)
        Builder.pexp_apply
          (Builder.pexp_ident (ghost (Ppxlib.Longident.Lident "&&")))
          [ (Nolabel, process_exp e1); (Nolabel, process_exp e2) ]
    | OrExp (e1, e2) ->
        (* orelse -> || *)
        Builder.pexp_apply
          (Builder.pexp_ident (ghost (Ppxlib.Longident.Lident "||")))
          [ (Nolabel, process_exp e1); (Nolabel, process_exp e2) ]
    | IfExp (e1, e2, e3) ->
        Builder.pexp_ifthenelse (process_exp e1) (process_exp e2)
          (Some (process_exp e3))
    | WhileExp (e1, e2) -> Builder.pexp_while (process_exp e1) (process_exp e2)
    | CaseExp (e, cases) ->
        Builder.pexp_match (process_exp e) (process_matching cases.value)
    | FnExp cases ->
        (* fn match -> function ... *)
        process_fun_exp cases.value
    end in
    (* Still call cite to accumulate comments, won't attach as attributes *)
    let result = labeller#cite Helpers.Attr.expression expression.pos res in
    (* Exit accumulation mode *)
    labeller#exit_accumulate_context;
    result

  (** Convert SML function expression to OCaml.
      Literal translation - currying is handled by ocaml.ml post-processing.
      fn cases -> function cases *)
  and process_fun_exp (cases : Ast.matching) : Parsetree.expression =
    Builder.pexp_function (process_matching cases)
  (** {2 Expression Rows and Matching}

      Helper functions for expression-related constructs. *)

  (** Convert an SML expression row (record field) to an OCaml record field.

      SML: [{x = 1, y = 2}] OCaml: [{x = 1; y = 2}]

      @param row The expression row (field binding)
      @return A pair of field identifier and expression *)
  and process_row (row : Ast.row Ast.node) :
      Ppxlib.Longident.t Location.loc * Parsetree.expression =
    let res = match row.value with
    | Row (lab, expression, rest_opt) ->
        let lab_longident =
          build_longident (idx_to_name lab.value)
        in
        (ghost lab_longident, process_exp expression) in
    res

  (** Convert SML match clauses to OCaml case list.

      SML: [pat1 => exp1 | pat2 => exp2] OCaml: [| pat1 -> exp1 | pat2 -> exp2]

      Used in [case], [fn], and [handle] expressions.

      @param m The match clause(s)
      @return A list of OCaml case expressions *)
  and process_matching (m : Ast.matching) : Parsetree.case list =
    
    let res = begin match m with
    | Case (pat, expression, rest_opt) -> (
        let case_here =
          Builder.case ~lhs:(process_pat pat) ~guard:None
            ~rhs:(process_exp expression)
        in
        match rest_opt with
        | None -> [ case_here ]
        | Some rest -> case_here :: process_matching rest.value)
      end in res
  (** {1 Pattern Processing}

      Functions for converting SML patterns to OCaml patterns.

      Pattern conversion is complex due to:
      - Distinguishing constructors from variables (SML uses capitalization, but
        also allows lowercase constructors in some contexts)
      - Handling the [op] keyword for treating infix operators as prefix
      - Converting record patterns to tuple/record patterns
      - Layered patterns ([x as pat]) *)

  (** Convert an SML pattern to an OCaml pattern.

    Handles all SML pattern forms including:
    - Constant patterns ([42], ["hello"])
    - Wildcard ([_])
    - Variable and constructor patterns (with heuristics for disambiguation)
    - Constructor application ([SOME x], [Cons(h, t)])
    - Infix constructors ([h :: t])
    - Tuples ([(x, y, z)])
    - Records ([{x, y}] or [{x = px, y = py}])
    - Lists ([[x, y, z]])
    - Type-annotated patterns ([x : int])
    - Layered patterns ([x as SOME y])

    @param is_head When true, treats identifier patterns as constructors.
                   Used in the head position of constructor applications.
    @param pat The SML pattern to convert
    @return The corresponding OCaml {!Parsetree.pattern}
    @raise Assert_failure For unimplemented pattern forms
    @raise WrongTypeName When identifier context is incorrect

    @example
    {[
      (* SML: case x of NONE => 0 | SOME y => y *)
      process_pat ~is_head:true (PatIdx (WithoutOp "NONE"))
      (* → Constructor pattern *)

      process_pat ~is_head:false (PatIdx (WithoutOp "y"))
      (* → Variable pattern *)
    ]} *)
  and pat_head_eq_ref (pat : Ast.with_op Ast.node) : bool =
    match pat.value with
    | WithOp _ -> false
    | WithoutOp id -> begin match id.value with
      | IdxIdx n -> n.value = "ref"
      | IdxLong [ n ] -> begin match n.value with
        | IdxIdx name -> name.value = "ref"
        | _ -> false
      end
      | _ -> false
    end
  and process_pat ?(is_arg = false) ?(is_head = false) (pat : Ast.pat Ast.node) :
      Parsetree.pattern =
    Log.log ~subgroup:"pattern" ~level:Debug ~kind:Neutral
      ~msg:(Printf.sprintf "Processing pattern: %s (is_head=%b, is_arg=%b)"
        (let dbg = Ast.show_pat pat.value in (match pat.value with
         | PatCon _ -> "PatCon"
         | PatWildcard -> "PatWildcard"
         | PatIdx _ -> "PatIdx"
         | PatApp _ -> "PatApp"
         | PatInfix _ -> "PatInfix"
         | PatParen _ -> "PatParen"
         | PatTuple _ -> "PatTuple"
         | PatRecord _ -> "PatRecord"
         | PatArray _ -> "PatArray"
         | PatList _ -> "PatList"
         | PatTyp _ -> "PatTyp"
         | PatAs _ -> "PatAs"
         | PatOr _ -> "PatOr") ^ " " ^ dbg)
        is_head is_arg)
      ();
    (* Enter accumulation mode for comment hoisting *)
    labeller#enter_accumulate_context;

    let res = begin match pat.value with
    | PatCon c -> Builder.ppat_constant (process_con c)
    | PatWildcard -> Builder.ppat_any
    | PatIdx wo -> (
        match wo.value with
        | WithOp op ->
            let op_name = idx_to_name op.value in
            let op_str = idx_to_string op.value in
            (* Extract module path if qualified *)
            let qual_path =
              if List.length op_name > 1 then
                Some (List.rev (List.tl (List.rev op_name)))
              else None
            in
            (* Try to look up as constructor *)
            let lookup_result = Constructor_registry.lookup
              Context.context.constructor_registry ~path:qual_path op_str in
            (match lookup_result with
            | Some ctor_info when is_head || not (is_variable_identifier op_str) ->
                (* It's a constructor - use transformed name *)
                let transformed_parts = match qual_path with
                  | Some path -> path @ [ctor_info.Constructor_registry.ocaml_name]
                  | None -> [ctor_info.ocaml_name]
                in
                let name_longident = build_longident transformed_parts in
                Builder.ppat_construct (ghost name_longident) None
            | _ ->
                (* It's a variable - force lowercase *)
                let name_str = Constructor_transform.transform_to_lowercase op_str in
                Builder.ppat_var (ghost name_str))
        | WithoutOp id ->
            let id_name = idx_to_name id.value in
            let id_str = idx_to_string id.value in
            (* Extract module path if qualified *)
            let qual_path =
              if List.length id_name > 1 then
                Some (List.rev (List.tl (List.rev id_name)))
              else None
            in
            (* Try to look up as constructor *)
            let lookup_result = Constructor_registry.lookup
              Context.context.constructor_registry ~path:qual_path id_str in
            (match lookup_result with
            | Some ctor_info when is_head || not (is_variable_identifier id_str) ->
                (* It's a constructor - use transformed name *)
                let transformed_parts = match qual_path with
                  | Some path -> path @ [ctor_info.Constructor_registry.ocaml_name]
                  | None -> [ctor_info.ocaml_name]
                in
                let name_longident = build_longident transformed_parts in
                Builder.ppat_construct (ghost name_longident) None
            | _ ->
                (* It's a variable - force lowercase *)
                let name_str = Constructor_transform.transform_to_lowercase id_str in
                Builder.ppat_var (ghost name_str)))
    | PatApp (node, p) when pat_head_eq_ref node -> let loc = Ppxlib.Location.none in ([%pat? { contents = [%p process_pat ~is_arg ~is_head p] }])
    | PatApp (wo, p) ->
        (* Constructor application: SOME x *)
        let const_name = process_with_op wo.value in
        let arg_pat = process_pat p in
        (* Look up constructor and use transformed name *)
        let lookup_result = Constructor_registry.lookup
          Context.context.constructor_registry ~path:None const_name in
        let final_name = match lookup_result with
          | Some ctor_info -> ctor_info.Constructor_registry.ocaml_name
          | None -> const_name
        in
        Builder.ppat_construct
          (ghost (build_longident [ final_name ]))
          (Some arg_pat)
    | PatInfix (p1, id, p2) ->
        (* Infix constructor pattern: x :: xs *)
        let op_name = idx_to_string id.value in
        (* Look up constructor and use transformed name *)
        let lookup_result = Constructor_registry.lookup
          Context.context.constructor_registry ~path:None op_name in
        let final_name = match lookup_result with
          | Some ctor_info -> ctor_info.Constructor_registry.ocaml_name
          | None -> op_name
        in
        let op_longident = build_longident [final_name] in
        let p1' = process_pat p1 in
        let p2' = process_pat p2 in
        Builder.ppat_construct (ghost op_longident)
          (Some (Builder.ppat_tuple [ p1'; p2' ]))
    | PatParen p -> process_pat p
    | PatTuple [] ->
        Builder.ppat_construct (ghost (Ppxlib.Longident.Lident "()")) None
    | PatTuple ps -> 
        let pat_list = process_tuple_pat is_arg ps in
        pat_list
    | PatRecord rows ->
        let fields =
          List.flatten (List.map (fun r -> process_pat_row r.Ast.value) rows)
        in
        Builder.ppat_record
          (List.map
             (fun (lab, pat) ->
               (ghost (build_longident [ lab ]), pat))
             fields)
          Closed
    | PatArray pats -> Builder.ppat_array (List.map (fun p -> process_pat ~is_arg ~is_head p) pats)
    | PatList pats ->
        (* Build list pattern from right to left *)
        List.fold_right
          (fun p acc ->
            Builder.ppat_construct
              (ghost (Ppxlib.Longident.Lident "::"))
              (Some (Builder.ppat_tuple [ process_pat ~is_arg ~is_head p; acc ])))
          pats
          (Builder.ppat_construct (ghost (Ppxlib.Longident.Lident "[]")) None)
    | PatTyp (p, t) -> Builder.ppat_constraint (process_pat ~is_arg ~is_head p) (process_type t)
    | PatAs (wo, t_opt, p) ->
        (* Layered pattern: x as SOME y *)
        let var_name = 
          match wo.value with
          | WithOp id -> idx_to_name id.value
          | WithoutOp id -> idx_to_name id.value
        in
        let var_str = process_lowercase (name_to_string var_name) in
        let inner_pat = process_pat ~is_arg ~is_head p in
        let final_pat =
          match t_opt with
          | None -> inner_pat
          | Some ty -> Builder.ppat_constraint inner_pat (process_type ty)
        in
        labeller#cite Helpers.Attr.pattern pat.pos
        @@ Builder.ppat_alias final_pat (ghost var_str)
        | PatOr(p1, p2) -> 
            let p1' = process_pat ~is_arg ~is_head p1 in
            let p2' = process_pat ~is_arg ~is_head p2 in
            Builder.ppat_or p1' p2'
    end in
    (* Still call cite to accumulate comments, won't attach as attributes *)
    let result = labeller#cite Helpers.Attr.pattern pat.pos res in
    (* Exit accumulation mode *)
    labeller#exit_accumulate_context;
    result
  (** Convert SML pattern rows (record pattern fields) to OCaml record patterns.

      SML record patterns have three forms:
      - Wildcard: [{..., x, y}] matches any record with at least x and y fields
      - Simple: [{x = px, y = py}] binds px and py
      - Variable shorthand: [{x, y}] is sugar for [{x = x, y = y}]

      @param row The pattern row to convert
      @return A list of field-pattern pairs
      @raise Assert_failure Currently unimplemented *)
  and process_pat_row (row : Ast.pat_row) : (string * Parsetree.pattern) list =
    let res = begin match row with
    | PatRowPoly ->
        (* Wildcard row - matches remaining fields *)
        (* No explicit field bindings for wildcard, return empty list *)
        []
    | PatRowSimple (lab, pat, rest) -> (
        let lab_str =
          name_to_string (idx_to_name lab.value)
        in
        let pat' = process_pat pat in
        let here = (lab_str, pat') in
        match rest.value with
        | PatRowPoly -> [ here ]
        | other -> here :: process_pat_row other)
    | PatRowVar (id, ty_opt, as_opt, rest_opt) -> (
        (* {x, y} is shorthand for {x = x, y = y} *)
        let id_str = name_to_string (idx_to_name id.value) in
        let var_pat = Builder.ppat_var (ghost id_str) in
        let pat_with_type =
          match ty_opt with
          | None -> var_pat
          | Some ty -> Builder.ppat_constraint var_pat (process_type ty)
        in
        let final_pat =
          match as_opt with
          | None -> pat_with_type
          | Some as_id ->
              let as_name =
                process_lowercase (name_to_string (idx_to_name as_id.value))
              in
              Builder.ppat_alias pat_with_type (ghost as_name)
        in
        let here = (id_str, final_pat) in
        match rest_opt with
        | None -> [ here ]
        | Some rest -> here :: process_pat_row rest.value)
    end in res
  and process_tuple_pat (is_arg : bool)
      (pats : Ast.pat Ast.node list) : Parsetree.pattern = Builder.ppat_tuple
    (List.map (fun p -> process_pat ~is_arg p) pats)

  and process_pat_many (pat : Ast.pat Ast.node) : Parsetree.pattern list =
    match pat.value with
    | PatTuple ps ->
        List.flatten (List.map (fun p -> process_pat_many p) ps)
    | _ -> [ process_pat pat ]
  (** {1 Declaration Processing}

      Functions for converting SML declarations to OCaml structure items.

      SML declarations include:
      - Value bindings ([val x = 42])
      - Function definitions ([fun f x = x + 1])
      - Type definitions ([type t = int * string])
      - Datatype declarations ([datatype t = A | B of int])
      - Exception declarations ([exception E of string])
      - Structure/module declarations
      - Local declarations, fixity declarations, etc. *)

  (** Convert an SML declaration to OCaml value bindings.

      Note: This function returns value_binding list, but some declarations
      (like type declarations) don't produce value bindings. Those return empty
      lists. For full program conversion, use process_prog which returns
      structure items.

      @param declaration The SML declaration to convert
      @return A list of OCaml value bindings *)
  and process_value_dec (declaration : Ast.declaration) :
      Parsetree.value_binding list =
    let res = begin
    match declaration with
    | ValDec (tvars, vb) ->
        (* Type variables in 'val' are currently ignored in conversion *)
        process_val_bind vb.value
    | FunDec fb -> process_fun_bind fb.value
    | TypDec tb ->
        (* Type declarations don't produce value bindings *)
        (* They should be handled at the structure level *)
        raise (mkBadAst ?loc:tb.pos "TypeDec is not value decleration")
    | DatDec (db, tb_opt) ->
        (* Datatype declarations don't produce value bindings *)
        (* They should be handled at the structure level *)
        raise (mkBadAst ?loc:db.pos "DatDec is not value decleration")
    | DataDecAlias (id1, id2) ->
        (* Datatype alias - no value bindings *)
        raise (mkBadAst "DataDecAlias is not value decleration")
    | AbstractDec (db, tb_opt, decs) ->
        (* Abstract type declarations *)
        (* Process the inner declarations *)
        List.concat (List.map (fun d -> process_value_dec d.Ast.value) decs)
    | ExnDec eb ->
        (* Exception declarations don't produce value bindings *)
        raise (mkBadAst ?loc:eb.pos "ExnDec is not value decleration")
    | StrDec sb ->
        (* Structure declarations don't produce value bindings *)
        raise (mkBadAst ?loc:sb.pos "StrDec is not value decleration")
    | SeqDec decs ->
        (* Sequential declarations - process each and concatenate *)
        List.concat (List.map (fun d -> process_value_dec d.Ast.value) decs)
    | LocalDec (d1, d2) -> raise (mkBadAst "LocalDec is not value decleration")
    | OpenDec [] ->
        raise (mkBadAst "OpenDec with empty list is not value decleration")
    | OpenDec (id :: rest) ->
        raise (mkBadAst ?loc:id.pos "OpenDec is not value decleration")
    | ExpDec exp ->
        (* Expression declarations don't produce value bindings *)
        raise (mkBadAst ?loc:exp.pos "ExpDec is not value decleration")
    | FixityDec (fix, ids) ->
        (* Fixity declarations don't produce value bindings *)
        raise (mkBadAst "FixityDec is not value decleration")
    end in res
  (** Convert SML fixity declarations to string representation.

      SML fixity: [infix 6 +], [infixr 5 ::], [nonfix f]

      @param fix The fixity specification
      @return String representation of fixity *)
  and process_fixity (fix : Ast.fixity) : string =
    match fix with
    | Nonfix -> "nonfix"
    | Infix n -> Printf.sprintf "infix %d" n.value
    | Infixr n -> Printf.sprintf "infixr %d" n.value

  (** Convert SML value bindings.

      SML: [val x = 42 and y = 43] OCaml: [let x = 42 and y = 43]

      @param vb The value binding(s)
      @return List of OCaml value bindings *)
  and process_val_bind (vb : Ast.value_binding) : Parsetree.value_binding list =
    Log.log ~subgroup:"binding" ~level:Debug ~kind:Neutral
      ~msg:"Processing value binding"
      ();
    match vb with
    | ValBind (pat, expression, rest_opt) ->
        let pat' = process_pat pat in
        let expression' = process_exp expression in
        let binding =
          labeller#cite Helpers.Attr.value_binding pat.pos
            (Builder.value_binding ~pat:pat' ~expr:expression')
        in
        let rest =
          match rest_opt with None -> [] | Some r -> process_val_bind r.value
        in
        binding :: rest
    | ValBindRec vb ->
        (* Recursive value bindings *)
        process_val_bind vb.value

  (** Convert SML function bindings to OCaml.

      SML: [fun f 0 = 1 | f n = n * f (n-1)] OCaml:
      [let rec f = function 0 -> 1 | n -> n * f (n-1)]

      @param fb The function binding(s)
      @return List of OCaml value bindings *)
  
  (** Check if all clauses have exactly one argument pattern *)
  and single_arg (clauses : (Parsetree.pattern list * Parsetree.expression) list) : bool =
    List.for_all (fun (pats, _) -> List.length pats = 1) clauses

  and process_fun_bind (fb : Ast.function_binding) :
      Parsetree.value_binding list =
    Log.log ~subgroup:"function" ~level:Debug ~kind:Neutral
      ~msg:"Processing function binding"
      ();
    match fb with
    | FunBind (fm, rest_opt) ->
        (* Get the function name from the first match *)
        let fname_str =
          match fm.value with
          | FunMatchPrefix (wo, _, _, _, _) -> process_with_op wo.value
          | FunMatchInfix (_, id, _, _, _, _) ->
              name_to_string (idx_to_name id.value)
          | FunMatchLow (_, id, _, _, _, _, _) ->
              name_to_string (idx_to_name id.value)
        in
        Log.log ~subgroup:"function" ~level:Debug ~kind:Neutral
          ~msg:(Printf.sprintf "Function name: %s" fname_str)
          ();

        (* Process all match clauses *)
        let clauses = process_fun_match fm.value in
        
        (* Build the function body *)
        let body =
          match clauses with
          | [] -> failwith "Function with no clauses"
          | [ (pats, expression) ] ->
              (* Single clause - build nested lambdas *)
              List.fold_right
                (fun pat acc -> Builder.pexp_fun Nolabel None pat acc)
                pats expression
          | clauses when single_arg clauses ->
              (* Multiple clauses with single argument - use function *)
              let cases =
                List.map
                  (fun (pats, expression) ->
                    let pat =
                      match pats with
                      | [ p ] -> p
                      | _ -> failwith "Expected single pattern"
                    in
                    Builder.case ~lhs:pat ~guard:None ~rhs:expression)
                  clauses
              in
              Builder.pexp_function cases
          | _ ->
              (* Multiple clauses - need pattern matching *)
              (* All clauses should have same number of parameters *)
              let num_params =
                match clauses with
                | (pats, _) :: _ -> List.length pats
                | [] -> 0
              in
              let get_temp = get_current_then num_params in
              (* Generate fresh parameter patterns *)
              let param_pats =
                List.init num_params (fun i ->
                    Builder.ppat_var (ghost (Printf.sprintf "arg__%d" (get_temp + i))))
              in
              (* Build function expression *)
              List.fold_right
                (fun pat acc -> Builder.pexp_fun Nolabel None pat acc)
                param_pats
                ((* Build match expression on tuple of arguments *)
                 let match_exp =
                   Builder.pexp_tuple
                     (List.init num_params (fun i ->
                          Builder.pexp_ident
                            (ghost
                               (Ppxlib.Longident.Lident
                                  (Printf.sprintf "arg__%d" (get_temp + i))))))
                 in
                 let cases =
                   List.map
                     (fun (pats, expression) ->
                       let pat = Builder.ppat_tuple pats in
                       Builder.case ~lhs:pat ~guard:None ~rhs:expression)
                     clauses
                 in
                 Builder.pexp_match match_exp cases)
        in

        let pat = Builder.ppat_var (ghost fname_str) in
        let binding =
          labeller#cite Helpers.Attr.value_binding fm.pos
            (Builder.value_binding ~pat ~expr:body)
        in

        let rest =
          match rest_opt with None -> [] | Some r -> process_fun_bind r.value
        in
        binding :: rest
  and 
  get_arity (pat : Ast.pat) : int = match pat with
    | PatTuple ps -> List.length ps
    | PatParen p -> get_arity p.value
    | _ -> 1
  (** Convert SML function match clauses.

      Helper for {!process_fun_bind}.

      @param fm The function match clause(s)
      @return List of pattern-expression pairs *)
  (** Convert SML function match clauses.
      Literal translation - currying is handled by ocaml.ml post-processing. *)
  and process_fun_match (fm : Ast.fun_match) :
      (Parsetree.pattern list * Parsetree.expression) list =
    match fm with
    | FunMatchPrefix (wo, pats, ty_opt, expression, rest_opt) ->
        (* fun f pat1 pat2 ... = expression *)
        let pats' = List.map (fun p -> process_pat p) pats in
        let expression' = process_exp expression in
        let exp_with_type =
          match ty_opt with
          | None -> expression'
          | Some ty -> Builder.pexp_constraint expression' (process_type ty)
        in
        let here = (pats', exp_with_type) in
        let rest =
          match rest_opt with None -> [] | Some r -> process_fun_match r.value
        in
        here :: rest
    | FunMatchInfix (p1, id, p2, ty_opt, expression, rest_opt) ->
        Log.log ~level:Common.Debug ~kind:Neutral
          ~msg:
            (Printf.sprintf "Processing infix function match with operator: %s"
               (idx_to_string id.value))
          ();
        (* fun p1 op p2 = expression - infix function *)
        let pats' = List.map (fun p -> process_pat p) [p1; p2] in
        let expression' = process_exp expression in
        let exp_with_type =
          match ty_opt with
          | None -> expression'
          | Some ty -> Builder.pexp_constraint expression' (process_type ty)
        in
        let here = (pats', exp_with_type) in
        let rest =
          match rest_opt with None -> [] | Some r -> process_fun_match r.value
        in
        here :: rest
    | FunMatchLow (p1, id, p2, pats, ty_opt, expression, rest_opt) ->
        Log.log ~level:Common.Debug ~kind:Neutral
          ~msg:
            (Printf.sprintf
               "Processing low-precedence infix function match with operator: %s"
               (idx_to_string id.value))
          ();
        (* fun (p1 op p2) pat3 ... = expression - curried infix *)
        let all_pats = List.map (fun p -> process_pat p) (p1 :: p2 :: pats) in
        let expression' = process_exp expression in
        let exp_with_type =
          match ty_opt with
          | None -> expression'
          | Some ty -> Builder.pexp_constraint expression' (process_type ty)
        in
        let here = (all_pats, exp_with_type) in
        let rest =
          match rest_opt with None -> [] | Some r -> process_fun_match r.value
        in
        here :: rest

  (** Convert SML type bindings (type abbreviations).

      SML: [type 'a pair = 'a * 'a] OCaml: [type 'a pair = 'a * 'a]

      @param tb The type binding(s)
      @return List of OCaml type declarations *)
  and process_typ_bind (tb : Ast.type_binding) : Parsetree.type_declaration list
      =
    match tb with
    | TypBind (tvars, id, ty, rest_opt) ->
        let name_str =
          name_to_string (idx_to_name id.value)
        in
        let params = Type_var_utils.process_type_params tvars in
        let manifest = Some (process_type ty) in
        let tdecl =
          labeller#cite Helpers.Attr.type_declaration id.pos
            (Builder.type_declaration ~name:(ghost name_str) ~params ~cstrs:[]
               ~kind:Parsetree.Ptype_abstract ~private_:Asttypes.Public
               ~manifest)
        in
        let rest =
          match rest_opt with None -> [] | Some r -> process_typ_bind r.value
        in
        tdecl :: rest

  (** Convert SML datatype bindings to OCaml variant types.

      SML: [datatype 'a option = NONE | SOME of 'a] OCaml:
      [type 'a option = None | Some of 'a]

      @param db The datatype binding(s)
      @return List of OCaml type declarations *)
  and process_dat_bind (db : Ast.data_binding) : Parsetree.type_declaration list
      =
    Log.log ~subgroup:"datatype" ~level:Debug ~kind:Neutral
      ~msg:"Processing datatype binding"
      ();
    match db with
    | DatBind (tvars, id, cb, rest_opt) ->
        let name_str =
          name_to_string (idx_to_name id.value)
        in
        Log.log ~subgroup:"datatype" ~level:Debug ~kind:Neutral
          ~msg:(Printf.sprintf "Datatype name: %s" name_str)
          ();
        let params = Type_var_utils.process_type_params tvars in
        let constructors = process_con_bind cb.value in
        let tdecl =
          labeller#cite Helpers.Attr.type_declaration id.pos
            (Builder.type_declaration ~name:(ghost name_str) ~params ~cstrs:[]
               ~kind:(Parsetree.Ptype_variant constructors)
               ~private_:Asttypes.Public ~manifest:None)
        in
        let rest =
          match rest_opt with None -> [] | Some r -> process_dat_bind r.value
        in
        tdecl :: rest

  (** Convert SML constructor bindings within a datatype.

      @param cb The constructor binding(s)
      @return List of OCaml constructor declarations *)
  and process_con_bind (cb : Ast.constructor_binding) :
      Parsetree.constructor_declaration list =
    match cb with
    | ConBind (id, ty_opt, rest_opt) ->
        let original_name = idx_to_name id.value in
        let name_str = name_to_string original_name in
        (* Register constructor in registry *)
        register_constructor name_str;
        let args =
          match ty_opt with
          | None -> Parsetree.Pcstr_tuple []
          | Some ty -> Parsetree.Pcstr_tuple [ process_type ty ]
        in
        let cdecl =
          labeller#cite Helpers.Attr.constructor_declaration id.pos
            (Builder.constructor_declaration ~name:(ghost name_str) ~args
               ~res:None)
        in
        let rest =
          match rest_opt with
          | None -> []
          | Some rest -> process_con_bind rest.value
        in
        cdecl :: rest

  (** Convert SML exception bindings.

      SML: [exception Empty] or [exception Fail of string] OCaml:
      [exception Empty] or [exception Fail of string]

      @param eb The exception binding(s)
      @return List of OCaml extension constructors *)
  and process_exn_bind (eb : Ast.exn_bind) :
      Parsetree.extension_constructor list =
    match eb with
    | ExnBind (id, ty_opt, rest_opt) ->
        let original_name = idx_to_name id.value in
        let name_str = name_to_string original_name in
        (* Register exception as constructor in registry *)
        register_constructor name_str;
        let args =
          match ty_opt with
          | None -> Parsetree.Pcstr_tuple []
          | Some ty -> Parsetree.Pcstr_tuple [ process_type ty ]
        in
        let ext_constr =
          labeller#cite Helpers.Attr.exception_constructor id.pos
            (Builder.extension_constructor ~name:(ghost name_str)
               ~kind:(Parsetree.Pext_decl ([], args, None)))
        in
        let rest =
          match rest_opt with None -> [] | Some r -> process_exn_bind r.value
        in
        ext_constr :: rest
    | ExnBindAlias (id1, id2, rest_opt) ->
        let name1_str =
          name_to_string (idx_to_name id1.value)
        in
        (* Register aliased exception as constructor *)
        register_constructor name1_str;
        let longid2 =
          build_longident (idx_to_name id2.value)
        in
        let ext_constr =
          labeller#cite Helpers.Attr.exception_constructor id1.pos
            (Builder.extension_constructor ~name:(ghost name1_str)
               ~kind:(Parsetree.Pext_rebind (ghost longid2)))
        in
        let rest =
          match rest_opt with None -> [] | Some r -> process_exn_bind r.value
        in
        ext_constr :: rest

  (** Extract identifier from SML [op] prefix wrapper (literal translation).

      The [op] keyword in SML removes infix status: [op +] is prefix [+].

      @param wo The identifier with or without [op]
      @return The identifier string *)
  and process_with_op (wo : Ast.with_op) : string =
    match wo with
    | WithOp id -> idx_to_string id.value
    | WithoutOp id -> idx_to_string id.value

  (** {1 Structure Processing}

      Functions for converting SML structures (modules) to OCaml modules.

      SML structures are first-class modules that can be:
      - Named and bound ([structure S = struct ... end])
      - Annotated with signatures ([S : SIG] or [S :> SIG])
      - Created via functor application ([F(A)])
      - Combined with local declarations *)

  (** Convert an SML structure expression to OCaml module items.

      Note: This function returns structure items (module contents) for cases
      where the structure can be inlined. For structure references and functor
      applications, this may not be fully accurate.

      @param structure The SML structure to convert
      @return List of OCaml structure items *)
  and process_str (structure : Ast.structure) : Parsetree.structure_item list =
    Log.log ~subgroup:"structure" ~level:Debug ~kind:Neutral
      ~msg:(Printf.sprintf "Processing structure: %s"
        (match structure with
         | StrIdx _ -> "StrIdx"
         | StructStr _ -> "StructStr"
         | AnotateStr _ -> "AnotateStr"
         | FunctorApp _ -> "FunctorApp"
         | FunctorAppAnonymous _ -> "FunctorAppAnonymous"
         | LocalDec _ -> "LocalDec"))
      ();
    let res = begin match structure with
    | StrIdx id ->
        let name =
          build_longident (idx_to_name id.value)
        in
        Builder.pstr_module
          (Builder.module_binding
             ~name:(ghost (Some (idx_to_string id.value)))
             ~expr:(Builder.pmod_ident (ghost name)))
        :: []
    | StructStr declaration ->
        (* struct declaration end - convert declarations to structure items *)
        dec_to_structure_items declaration.value
    | AnotateStr (_id, _annot, s) ->
        (* Annotated structure - just process the inner structure *)
        (* The annotation would be handled at binding site *)
        process_str s.value
    | FunctorApp (id, s) ->
        (* Functor application - can't inline *)
        let functor_id =
          build_longident (idx_to_name id.value)
        in
        let arg_expr = structure_to_module_expr s.value in
        let mod_expr =
          Builder.pmod_apply (Builder.pmod_ident (ghost functor_id)) arg_expr
        in
        Builder.pstr_module
          (Builder.module_binding
             ~name:(ghost (Some (idx_to_string id.value)))
             ~expr:mod_expr)
        :: []
    | FunctorAppAnonymous (_id, declaration) ->
        (* Functor applied to anonymous struct *)
        dec_to_structure_items declaration.value
    | LocalDec (declaration, s) ->
        (* Local declarations in structure *)
        dec_to_structure_items declaration.value @ process_str s.value
    end in 
    res
  (** Convert SML signature annotation type.

      - Transparent ([:]): Type equalities visible
      - Opaque ([:>]): Abstract types hidden

      @param a The annotation type
      @return String representation *)
  and process_anotate (a : Ast.anotate) : string =
    match a with Transparent -> ":" | Opaque -> ":>"

  (** Convert an SML structure to an OCaml module expression.

      @param structure The SML structure to convert
      @return An OCaml module expression *)
  and structure_to_module_expr (structure : Ast.structure) :
      Parsetree.module_expr =
    match structure with
    | StrIdx id ->
        (* Structure reference - becomes module identifier *)
        let longid =
          build_longident (idx_to_name id.value)
        in
        labeller#cite Helpers.Attr.module_expr id.pos
          (Builder.pmod_ident (ghost longid))
    | StructStr declaration ->
        (* struct ... end - convert declarations to structure *)
        let items = dec_to_structure_items declaration.value in
        Builder.pmod_structure items
    | AnotateStr (_id, annot, s) ->
        (* Annotated structure *)
        let inner = structure_to_module_expr s.value in
        (* Note: annotation is typically handled at binding site *)
        inner
    | FunctorApp (id, s) ->
        (* Functor application F(A) *)
        let functor_id =
          build_longident (idx_to_name id.value)
        in
        let arg_expr = structure_to_module_expr s.value in
        Builder.pmod_apply (Builder.pmod_ident (ghost functor_id)) arg_expr
    | FunctorAppAnonymous (_id, declaration) ->
        (* Functor applied to anonymous struct *)
        let items = dec_to_structure_items declaration.value in
        Builder.pmod_structure items
    | LocalDec (declaration, s) ->
        (* Local declarations in structure: let <dec> in <struct> end *)
        (* This doesn't map cleanly to OCaml module expressions *)
        (* For now, combine the declarations with the structure *)
        let local_items = dec_to_structure_items declaration.value in
        let struct_items =
          match s.value with
          | StructStr d -> dec_to_structure_items d.value
          | _ ->
              (* For non-struct cases, we need to convert to items *)
              (* This is a simplification - may need refinement *)
              let mod_expr = structure_to_module_expr s.value in
              [
                Builder.pstr_module
                  (Builder.module_binding ~name:(ghost (Some "_local"))
                     ~expr:mod_expr);
              ]
        in
        Builder.pmod_structure (local_items @ struct_items)

  (** Convert SML structure bindings.

      SML: [structure S = struct ... end] OCaml: [module S = struct ... end]

      @param sb The structure binding(s)
      @return List of OCaml module bindings *)
  and process_str_bind (sb : Ast.structure_binding) :
      Parsetree.module_binding list =
    trace_part ~level:2 ~ast:"structure_binding"
      ~msg:"" (* ~msg:(Ast.show_str_bind sb) *) ~value:(fun () ->
        match sb with
        | StrBind (id, annot_opt, structure, rest_opt) ->
            let name_str =
              name_to_string (idx_to_name id.value)
            in
            (* Convert the structure body to a module expression *)
            let module_expr = structure_to_module_expr structure.value in
            let module_expr_with_sig =
              match annot_opt with
              | None -> module_expr
              | Some (annot, signature) -> (
                  let module_type = process_sign signature.value in
                  match annot.value with
                  | Transparent ->
                      Builder.pmod_constraint module_expr module_type
                  | Opaque -> Builder.pmod_constraint module_expr module_type)
            in
            let binding =
              labeller#cite Helpers.Attr.module_binding id.pos
                (Builder.module_binding ~name:(ghost (Some name_str))
                   ~expr:module_expr_with_sig)
            in
            let rest =
              match rest_opt with
              | None -> []
              | Some r -> process_str_bind r.value
            in
            binding :: rest)

  (** {1 Signature Processing}

      Functions for converting SML signatures (module types) to OCaml
      signatures.

      SML signatures specify the interface of structures, including:
      - Value specifications ([val f : int -> int])
      - Type specifications ([type t], [eqtype t])
      - Datatype specifications
      - Exception specifications
      - Nested structure specifications
      - Sharing constraints ([sharing type t1 = t2]) *)

  (** Convert an SML signature to OCaml signature items.

      @param signature The SML signature to convert
      @return List of OCaml signature items *)
  and process_sign (signature : Ast.signature) : Parsetree.module_type =
    Log.log ~subgroup:"signature" ~level:Debug ~kind:Neutral
      ~msg:(Printf.sprintf "Processing signature: %s"
        (match signature with
         | SignIdx _ -> "SignIdx"
         | SignSig _ -> "SignSig"
         | SignWhere _ -> "SignWhere"))
      ();
    trace_part ~level:2 ~ast:"signature"
      ~msg:"" (* ~msg:(Ast.show_sign signature) *) ~value:(fun () ->
        match signature with
        | SignIdx id ->
            (* Signature identifier - can't inline, needs module type context *)
            (* This should ideally be handled at module type level *)
            let longid =
              build_longident (idx_to_name id.value)
            in
            labeller#cite Helpers.Attr.module_type id.pos
              (Builder.pmty_ident (ghost longid))
        | SignSig specification ->
            (* sig specification end - process specifications *)
            let specification' =
              List.flatten (List.map process_spec specification)
            in
            Builder.pmty_signature specification'
        | SignWhere (s, _tr) ->
            (* Signature with where clauses *)
            (* Where clauses should be handled at module type level *)
            (* For now, just process the base signature *)
            process_sign s.value)

  (** Convert SML type refinement ([where type]) clauses.

      SML: [sig ... end where type t = int] OCaml: Uses [with type] constraints

      @param tr The type refinement
      @return List of type identifier-definition pairs *)
  and process_typ_refine (tr : Ast.typ_refine) :
      (Ppxlib.Longident.t * Parsetree.core_type) list =
    match tr with
    | TypRef (_tvars, id, ty, rest_opt) ->
        (* Type variables are currently ignored in refinement *)
        let longid =
          build_longident (idx_to_name id.value)
        in
        let core_type = process_type ty in
        let here = (longid, core_type) in
        let rest =
          match rest_opt with
          | None -> []
          | Some (_ty, tr_rest) -> process_typ_refine tr_rest.value
        in
        here :: rest

  (** Convert SML specifications within signatures.

      @param specification The specification to convert
      @return List of OCaml signature items *)
  and process_spec (specification' : Ast.specification Ast.node) :
      Parsetree.signature_item list =
    trace_part ~level:2 ~ast:"specification"
      ~msg:"" (* ~msg:(let _ = Ast.show_spec specification'.value in "") *)
      ~value:(fun () ->
        match specification'.value with
        | SpecVal vd ->
            let vdescs = process_val_specification vd.value in
            List.map (fun vd -> Builder.psig_value vd) vdescs
        | SpecTyp td ->
            let tdecls = process_typ_specification td.value in
            [ Builder.psig_type Asttypes.Nonrecursive tdecls ]
        | SpecEqtyp td ->
            (* Equality types - in OCaml just abstract types *)
            let tdecls = process_typ_specification td.value in
            [ Builder.psig_type Asttypes.Nonrecursive tdecls ]
        | SpecTypBind tb ->
            let tdecls = process_typ_bind tb.value in
            [ Builder.psig_type Asttypes.Nonrecursive tdecls ]
        | SpecDat dd ->
            let tdecls = process_dat_specification dd.value in
            [ Builder.psig_type Asttypes.Recursive tdecls ]
        | SpecDatAlias (id1, id2) ->
            (* Datatype alias in signature *)
            let name1_str =
              name_to_string (idx_to_name id1.value)
            in
            let longid2 =
              build_longident (idx_to_name id2.value)
            in
            let alias_type = Builder.ptyp_constr (ghost longid2) [] in
            let tdecl =
              labeller#cite Helpers.Attr.type_declaration id1.pos
                (Builder.type_declaration ~name:(ghost name1_str) ~params:[]
                   ~cstrs:[] ~kind:Parsetree.Ptype_abstract
                   ~private_:Asttypes.Public ~manifest:(Some alias_type))
            in
            [ Builder.psig_type Asttypes.Recursive [ tdecl ] ]
        | SpecExn ed ->
            let ext_constrs = process_exn_specification ed.value in
            List.map
              (fun ec ->
                let type_exn =
                  labeller#cite Helpers.Attr.exception_declaration ed.pos
                    (Builder.type_exception ec)
                in
                Builder.psig_exception type_exn)
              ext_constrs
        | SpecStr sd ->
            let mdecls = process_str_specification sd.value in
            List.map (fun md -> Builder.psig_module md) mdecls
        | SpecSeq (s1, s2) -> List.append (process_spec s1) (process_spec s2)
        | SpecInclude s ->
            let module_type = process_sign s.value in
            let incl_info =
              labeller#cite Helpers.Attr.include_infos specification'.pos
                (Builder.include_infos module_type)
            in
            [ Builder.psig_include incl_info ]
        | SpecIncludeIdx ids ->
            List.concat
              (List.map
                 (fun (id : Ast.idx Ast.node) ->
                   let longid =
                     build_longident
                       (idx_to_name id.value)
                   in
                   let module_type = Builder.pmty_ident (ghost longid) in
                   let incl_info =
                     labeller#cite Helpers.Attr.include_infos id.pos
                       (Builder.include_infos module_type)
                   in
                   [ Builder.psig_include incl_info ])
                 ids)
        | SpecSharingTyp (s, _ids) ->
            (* Type sharing: SML [spec sharing type t1 = t2 = ...]
               OCaml lacks direct signature-level type sharing.
               The constraint is enforced at functor/module application. *)
            process_spec s
        | SpecSharingStr (s, _ids) ->
            (* Structure sharing: SML [spec sharing S1 = S2 = ...]
               OCaml lacks structure sharing constraints.
               The constraint is enforced at functor/module application. *)
            process_spec s)

  (** Convert SML value descriptions in signatures.

      @param vd The value description(s)
      @return List of OCaml value descriptions *)
  and process_val_specification (vd : Ast.val_specification) :
      Parsetree.value_description list =
    trace_part ~level:2 ~ast:"val_specification"
      ~msg:"" (* ~msg:(Ast.show_val_specification vd) *) ~value:(fun () ->
        match vd with
        | ValDesc (id, ty, rest_opt) ->
            let name_str =
              name_to_string (idx_to_name id.value)
            in
            let core_type = process_type ty in
            let vdesc =
              labeller#cite Helpers.Attr.value_description id.pos
                (Builder.value_description ~name:(ghost name_str)
                   ~type_:core_type ~prim:[])
            in
            let rest =
              match rest_opt with
              | None -> []
              | Some r -> process_val_specification r.value
            in
            vdesc :: rest)

  (** Convert SML abstract type descriptions.

      @param td The type description(s)
      @return List of OCaml type declarations *)
  and process_typ_specification (td : Ast.typ_specification) :
      Parsetree.type_declaration list =
    trace_part ~level:2 ~ast:"typ_specification"
      ~msg:"" (* ~msg:(Ast.show_typ_specification td) *) ~value:(fun () ->
        match td with
        | TypDesc (tvars, id, rest_opt) ->
            let name_str =
              name_to_string (idx_to_name id.value)
            in
            let params = Type_var_utils.process_type_params tvars in
            let tdecl =
              labeller#cite Helpers.Attr.type_declaration id.pos
                (Builder.type_declaration ~name:(ghost name_str) ~params
                   ~cstrs:[] ~kind:Parsetree.Ptype_abstract
                   ~private_:Asttypes.Public ~manifest:None)
            in
            let rest =
              match rest_opt with
              | None -> []
              | Some r -> process_typ_specification r.value
            in
            tdecl :: rest)

  (** Convert SML datatype descriptions in signatures.

      @param dd The datatype description(s)
      @return List of OCaml type declarations *)
  and process_dat_specification (dd : Ast.dat_specification) :
      Parsetree.type_declaration list =
    trace_part ~level:2 ~ast:"dat_specification"
      ~msg:"" (* ~msg:(Ast.show_dat_specification dd) *) ~value:(fun () ->
        match dd with
        | DatDesc (tvars, id, cd, rest_opt) ->
            let name_str =
              name_to_string (idx_to_name id.value)
            in
            let params = Type_var_utils.process_type_params tvars in
            let constructors = process_con_specification cd.value in
            let tdecl =
              labeller#cite Helpers.Attr.type_declaration id.pos
                (Builder.type_declaration ~name:(ghost name_str) ~params
                   ~cstrs:[] ~kind:(Parsetree.Ptype_variant constructors)
                   ~private_:Asttypes.Public ~manifest:None)
            in
            let rest =
              match rest_opt with
              | None -> []
              | Some r -> process_dat_specification r.value
            in
            tdecl :: rest)

  (** Convert SML constructor descriptions in signatures.

      @param cd The constructor description(s)
      @return List of OCaml constructor declarations *)
  and process_con_specification (cd : Ast.con_specification) :
      Parsetree.constructor_declaration list =
    trace_part ~level:2 ~ast:"con_specification"
      ~msg:"" (* ~msg:(Ast.show_con_specification cd) *) ~value:(fun () ->
        match cd with
        | ConDesc (id, ty_opt, rest_opt) ->
            (* Same as process_con_bind *)
            let name_str =
              name_to_string (idx_to_name id.value)
            in
            let args =
              match ty_opt with
              | None -> Parsetree.Pcstr_tuple []
              | Some ty -> Parsetree.Pcstr_tuple [ process_type ty ]
            in
            let cdecl =
              labeller#cite Helpers.Attr.constructor_declaration id.pos
                (Builder.constructor_declaration ~name:(ghost name_str) ~args
                   ~res:None)
            in
            let rest =
              match rest_opt with
              | None -> []
              | Some r -> process_con_specification r.value
            in
            cdecl :: rest)

  (** Convert SML exception descriptions in signatures.

      @param ed The exception description(s)
      @return List of OCaml extension constructors *)
  and process_exn_specification (ed : Ast.exn_specification) :
      Parsetree.extension_constructor list =
    trace_part ~level:2 ~ast:"exn_specification"
      ~msg:"" (* ~msg:(Ast.show_exn_specification ed) *) ~value:(fun () ->
        match ed with
        | ExnDesc (id, ty_opt, rest_opt) ->
            (* Similar to process_exn_bind but for signatures *)
            let name_str =
              name_to_string (idx_to_name id.value)
            in
            let args =
              match ty_opt with
              | None -> Parsetree.Pcstr_tuple []
              | Some ty -> Parsetree.Pcstr_tuple [ process_type ty ]
            in
            let ext_constr =
              labeller#cite Helpers.Attr.exception_constructor id.pos
                (Builder.extension_constructor ~name:(ghost name_str)
                   ~kind:(Parsetree.Pext_decl ([], args, None)))
            in
            let rest =
              match rest_opt with
              | None -> []
              | Some r -> process_exn_specification r.value
            in
            ext_constr :: rest)

  (** Convert SML structure descriptions in signatures.

      @param sd The structure description(s)
      @return List of OCaml module declarations *)
  and process_str_specification (sd : Ast.str_specification) :
      Parsetree.module_declaration list =
    let res = trace_part ~level:2 ~ast:"str_specification"
      ~msg:"" (* ~msg:(Ast.show_str_specification sd) *) ~value:(fun () ->
        match sd with
        | StrDesc (id, s, rest_opt) ->
            let name_str =
              name_to_string (idx_to_name id.value)
            in
            let module_type = process_sign s.value in
            let mdecl =
              labeller#cite Helpers.Attr.module_declaration id.pos
                (Builder.module_declaration ~name:(ghost (Some name_str))
                   ~type_:module_type)
            in
            let rest =
              match rest_opt with
              | None -> []
              | Some r -> process_str_specification r.value
            in
            mdecl :: rest)
          in 
          res

  (** {1 Program Processing}

      Functions for converting top-level SML programs.

      A program is a sequence of:
      - Core declarations
      - Functor declarations ([functor F(X: S) = ...])
      - Signature declarations ([signature S = ...]) *)

  (** Convert a declaration to structure items. Helper function for
      process_prog. *)
  and dec_to_structure_items (declaration : Ast.declaration) :
      Parsetree.structure_item list =
    Log.log ~subgroup:"declaration" ~level:Debug ~kind:Neutral
      ~msg:(Printf.sprintf "Converting declaration to structure items: %s"
        (match declaration with
         | ValDec _ -> "ValDec"
         | FunDec _ -> "FunDec"
         | TypDec _ -> "TypDec"
         | DatDec _ -> "DatDec"
         | DataDecAlias _ -> "DataDecAlias"
         | AbstractDec _ -> "AbstractDec"
         | ExnDec _ -> "ExnDec"
         | StrDec _ -> "StrDec"
         | SeqDec _ -> "SeqDec"
         | LocalDec _ -> "LocalDec"
         | OpenDec _ -> "OpenDec"
         | ExpDec _ -> "ExpDec"
         | FixityDec _ -> "FixityDec"))
      ();
    trace_part ~level:5 ~ast:"declaration"
      ~msg:"" (* ~msg:(Ast.show_dec declaration) *) ~value:(fun () ->
        match declaration with
        | ValDec (tvars, vb) ->
            let bindings = process_val_bind vb.value in
            List.map
              (fun binding ->
                Builder.pstr_value Asttypes.Nonrecursive [ binding ])
              bindings
        | FunDec fb ->
            let bindings = process_fun_bind fb.value in
            List.map
              (fun binding -> Builder.pstr_value Asttypes.Recursive [ binding ])
              bindings
        | TypDec tb ->
            let tdecls = process_typ_bind tb.value in
            [ Builder.pstr_type Asttypes.Nonrecursive tdecls ]
        | DatDec (db, tb_opt) -> (
            let tdecls = process_dat_bind db.value in
            let type_item = Builder.pstr_type Asttypes.Recursive tdecls in
            match tb_opt with
            | None -> [ type_item ]
            | Some tb ->
                let tb_decls = process_typ_bind tb.value in
                [ type_item; Builder.pstr_type Asttypes.Recursive tb_decls ])
        | DataDecAlias (id1, id2) ->
            (* Datatype alias: datatype t = datatype u *)
            (* In OCaml, this would be: type t = u *)
            let name1_str =
              name_to_string (idx_to_name id1.value)
            in
            let longid2 =
              build_longident (idx_to_name id2.value)
            in
            let alias_type = Builder.ptyp_constr (ghost longid2) [] in
            let tdecl =
              labeller#cite Helpers.Attr.type_declaration id1.pos
                (Builder.type_declaration ~name:(ghost name1_str) ~params:[]
                   ~cstrs:[] ~kind:Parsetree.Ptype_abstract
                   ~private_:Asttypes.Public ~manifest:(Some alias_type))
            in
            [ Builder.pstr_type Asttypes.Recursive [ tdecl ] ]
        | AbstractDec (db, tb_opt, decs) ->
            (* Abstract type with local implementations *)
            (* The datatype is abstract, only the inner decs are visible *)
            List.concat
              (List.map (fun d -> dec_to_structure_items d.Ast.value) decs)
        | ExnDec eb ->
            let exn_constrs = process_exn_bind eb.value in
            List.map
              (fun ec ->
                let type_exn =
                  labeller#cite Helpers.Attr.exception_declaration eb.pos
                    (Builder.type_exception ec)
                in
                Builder.pstr_exception type_exn)
              exn_constrs
        | StrDec sb ->
            let module_bindings = process_str_bind sb.value in
            List.map (fun mb -> Builder.pstr_module mb) module_bindings
        | SeqDec decs ->
            List.concat
              (List.map (fun d -> dec_to_structure_items d.Ast.value) decs)
        | LocalDec (d1, d2) ->
            (* Local declarations - both visible at top level in OCaml *)
            dec_to_structure_items d1.value @ dec_to_structure_items d2.value
        | OpenDec ids ->
            List.map
              (fun (id : Ast.idx Ast.node) ->
                let longid =
                  build_longident (idx_to_name id.value)
                in
                let module_expr = Builder.pmod_ident (ghost longid) in
                let open_info =
                  labeller#cite Helpers.Attr.open_infos id.pos
                    (Builder.open_infos ~override:Asttypes.Fresh ~expr:module_expr)
                in
                Builder.pstr_open open_info)
              ids
        | ExpDec exp ->
            (* Top-level expression - convert to a structure item *)
            let ocaml_expr = process_exp exp in
            [ Builder.pstr_eval ocaml_expr [] ]
        | FixityDec (_fix, _ids) -> [])

  (** Convert a top-level SML program to an OCaml structure.

      @param prog The SML program to convert
      @return An OCaml structure (list of structure items) *)
  and process_prog (prog : Ast.prog) : Parsetree.structure =
    Log.log ~subgroup:"program" ~level:Medium ~kind:Neutral
      ~msg:(Printf.sprintf "Processing program: %s"
        (match prog with
         | ProgDec _ -> "ProgDec"
         | ProgFun _ -> "ProgFun"
         | ProgStr _ -> "ProgStr"
         | ProgSeq _ -> "ProgSeq"
         | ProgEmpty -> "ProgEmpty"))
      ();
    let res = trace_part ~level:5 ~ast:"prog" ~msg:"" ~value:(fun () ->
        match prog with
        | ProgDec declaration -> dec_to_structure_items declaration.value
        | ProgFun fb ->
            let module_bindings = process_functor_binding fb.value in
            List.map (fun mb -> Builder.pstr_module mb) module_bindings
        | ProgStr sb ->
            let mtdecls = process_signature_binding sb.value in
            List.map (fun mtd -> Builder.pstr_modtype mtd) mtdecls
        | ProgSeq (p1, p2) -> process_prog p1.value @ process_prog p2.value
        | ProgEmpty -> []) in
    res

  (** Convert SML functor bindings (parameterized modules).

      SML: [functor F(X : SIG) = struct ... end] OCaml:
      [module F(X : SIG) = struct ... end]

      @param fb The functor binding(s)
      @return List of OCaml module bindings *)
  and process_functor_binding (fb : Ast.functor_binding) :
      Parsetree.module_binding list =
    Log.log ~subgroup:"functor" ~level:Debug ~kind:Neutral
      ~msg:(Printf.sprintf "Processing functor binding: %s"
        (match fb with
         | FctBind _ -> "FctBind"
         | FctBindOpen _ -> "FctBindOpen"
         | FctGen _ -> "FctGen"))
      ();
    let res = trace_part ~level:2 ~ast:"functor_binding"
      ~msg:"" (* ~msg:(Ast.show_functor_binding fb) *) ~value:(fun () ->
        let res =
          match fb with
          | FctBind (name, param, sig1, annot_opt, body, rest_opt) ->
              let fname_str =
                name_to_string (idx_to_name name.value)
              in
              let pname_str =
                name_to_string (idx_to_name param.value)
              in

              (* Process parameter signature *)
              let param_module_type = process_sign sig1.value in

              (* Process functor body *)
              let body_items = process_str body.value in
              let body_module_expr = Builder.pmod_structure body_items in

              (* Add result signature constraint if present *)
              let final_body =
                match annot_opt with
                | None -> body_module_expr
                | Some (_annot, result_sig) ->
                    let result_module_type = process_sign result_sig.value in
                    Builder.pmod_constraint body_module_expr result_module_type
              in

              (* Create functor *)
              let functor_param =
                Parsetree.Named (ghost (Some pname_str), param_module_type)
              in
              let functor_expr =
                Builder.pmod_functor functor_param final_body
              in

              let binding =
                labeller#cite Helpers.Attr.module_binding name.pos
                  (Builder.module_binding ~name:(ghost (Some fname_str))
                     ~expr:functor_expr)
              in

              let rest =
                match rest_opt with
                | None -> []
                | Some r -> process_functor_binding r.value
              in
              binding :: rest
          | FctBindOpen (name, specification, annot_opt, body, rest_opt) ->
              (* Opened functor - parameter specification is directly visible *)
              let fname_str =
                name_to_string (idx_to_name name.value)
              in

              (* Process parameter specification *)
              let param_module_spec = process_spec specification in
              let param_module_type =
                Builder.pmty_signature param_module_spec
              in

              (* Process functor body *)
              let body_items = process_str body.value in
              let body_module_expr = Builder.pmod_structure body_items in

              (* Add result signature constraint if present *)
              let final_body =
                match annot_opt with
                | None -> body_module_expr
                | Some (_annot, result_sig) ->
                    let result_module_type = process_sign result_sig.value in
                    Builder.pmod_constraint body_module_expr result_module_type
              in

              (* Create functor with unit parameter (opened specs) *)
              let functor_param =
                Parsetree.Named (ghost (Some fname_str), param_module_type)
              in
              let functor_expr =
                Builder.pmod_functor functor_param final_body
              in

              let binding =
                labeller#cite Helpers.Attr.module_binding name.pos
                  (Builder.module_binding ~name:(ghost (Some fname_str))
                     ~expr:functor_expr)
              in

              let rest =
                match rest_opt with
                | None -> []
                | Some r -> process_functor_binding r.value
              in
              binding :: rest
          | FctGen (idx, annotate, str, rest_opt) ->
              let fname_str =
                name_to_string (idx_to_name idx.value)
              in

              (* Process parameter signature *)
              let param_module_type =
                match annotate with
                | None ->
                    (* Handle case when there is no annotation *)
                    (* You might want to define a default or raise an error *)
                    (* For now, let's assume a default empty signature *)
                    Builder.pmty_signature []
                | Some (_annot, sig1) -> process_sign sig1.value
              in
              (* Process functor body *)
              let body_items = process_str str.value in
              let body_module_expr = Builder.pmod_structure body_items in

              (* Add result signature constraint if present *)
              let final_body =
                match annotate with
                | None -> body_module_expr
                | Some (_annot, result_sig) ->
                    let result_module_type = process_sign result_sig.value in
                    Builder.pmod_constraint body_module_expr result_module_type
              in

              (* Create functor *)
              let functor_param = Parsetree.Unit in
              let functor_expr =
                Builder.pmod_functor functor_param final_body
              in

              let binding =
                labeller#cite Helpers.Attr.module_binding idx.pos
                  (Builder.module_binding ~name:(ghost (Some fname_str))
                     ~expr:functor_expr)
              in

              let rest =
                match rest_opt with
                | None -> []
                | Some r -> process_functor_binding r.value
              in
              binding :: rest
        in
        res)
      in 
    res

  (** Convert SML signature bindings.

      SML: [signature SIG = sig ... end] OCaml: [module type SIG = sig ... end]

      @param sb The signature binding(s)
      @return List of OCaml module type declarations *)
  and process_signature_binding (sb : Ast.signature_binding) :
      Parsetree.module_type_declaration list =
    trace_part ~level:2 ~ast:"signature_binding"
      ~msg:"" (* ~msg:(Ast.show_signature_binding sb) *) ~value:(fun () ->
        match sb with
        | SignBind (id, s, rest_opt) ->
            let name_str =
              name_to_string (idx_to_name id.value)
            in
            let module_type = process_sign s.value in
            let mtdecl =
              labeller#cite Helpers.Attr.module_type_declaration id.pos
                (Builder.module_type_declaration ~name:(ghost name_str)
                   ~type_:(Some module_type))
            in
            let rest =
              match rest_opt with
              | None -> []
              | Some r -> process_signature_binding r.value
            in
            mtdecl :: rest)

  (** Main entry point for converting a complete SML program. Wraps the
      converted structure in a toplevel phrase for output. *)
  and process_sml ~(prog : Ast.prog) : res =
    let output_src =
      match Common.get_verbosity config with None -> false | Some n -> n >= 2
    in
    if output_src then Stdlib.Format.eprintf "@,Lexical source: @[%s@]@," lexbuf;
    let structure = process_prog prog in
    let _ = labeller#destruct () in
    [ Parsetree.Ptop_def structure ]
end
