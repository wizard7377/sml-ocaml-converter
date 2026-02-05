(** Precedence Resolver - Restructures flat expression sequences according to SML operator precedence.

    This module implements a two-phase operator precedence strategy:
    1. Parser produces flat sequences (e.g., [1; +; 2; *; 3])
    2. Backend resolves precedence to build correct tree (e.g., 1 + (2 * 3))

    @see ast_core.ml ExpApp for flat sequence representation *)

open Ast

(** Operator associativity *)
type assoc = Left | Right

(** Standard SML operator precedence table.
    Higher precedence binds tighter. *)
let precedence_table : (int * assoc * string list) list = [
  (7, Left,  ["*"; "/"; "div"; "mod"]);
  (6, Left,  ["+"; "-"; "^"]);
  (5, Right, ["::"; "@"]);
  (4, Left,  ["="; "<>"; ">"; ">="; "<"; "<="]);
  (3, Left,  [":="; "o"]);
  (0, Left,  ["before"]);
]

(** Check if a character is an operator character.
    Uses SML's syntactic convention for operator identifiers. *)
let is_operator_char (c : char) : bool =
  match c with
  | '!' | '%' | '&' | '$' | '#' | '+' | '-' | '/' | ':'
  | '<' | '=' | '>' | '?' | '@' | '\\' | '~' | '`' | '^' | '|' | '*' -> true
  | _ -> false

(** Check if a name represents an operator using syntactic heuristics. *)
let is_operator (name : string) : bool =
  String.length name > 0 && is_operator_char name.[0]

(** Look up operator precedence and associativity.
    Returns None if not in the table (treat as value, not operator). *)
let get_precedence (op_name : string) : (int * assoc) option =
  let rec search = function
    | [] -> None
    | (prec, assoc, ops) :: rest ->
        if List.mem op_name ops then Some (prec, assoc)
        else search rest
  in
  search precedence_table

(** Resolved expression structure after precedence resolution. *)
type resolved_exp =
  | ResolvedSingle of expression
      (** Single expression with no operators *)
  | ResolvedApp of resolved_exp * expression node list
      (** Left-associative function application: f x y z → ((f x) y) z *)
  | ResolvedInfix of resolved_exp * idx node * resolved_exp
      (** Binary operator application: x + y *)

(** Extract operator name from an expression node.
    Returns None if the expression is not an operator. *)
let extract_operator (exp : expression node) : idx node option =
  match exp.value with
  | ExpIdx idx ->
      let name = Idx_utils.idx_to_string idx.value in
      if is_operator name then Some idx else None
  | _ -> None

(** Find all operator positions in a sequence with their precedence info.
    Returns list of (position, precedence, associativity, operator_node). *)
let find_operators (items : expression node list) : (int * int * assoc * idx node) list =
  let rec find_at pos = function
    | [] -> []
    | exp :: rest ->
        (match extract_operator exp with
        | Some op_idx ->
            let op_name = Idx_utils.idx_to_string op_idx.value in
            (match get_precedence op_name with
            | Some (prec, assoc) ->
                (pos, prec, assoc, op_idx) :: find_at (pos + 1) rest
            | None ->
                (* Not a recognized operator, treat as value *)
                find_at (pos + 1) rest)
        | None ->
            find_at (pos + 1) rest)
  in
  find_at 0 items

(** Find the position to split the sequence for precedence resolution.

    Strategy:
    - Find operator with lowest precedence
    - Among same precedence:
      - Left-assoc: pick RIGHTMOST (so left side evaluates first)
      - Right-assoc: pick LEFTMOST (so right side evaluates first) *)
let find_split_position (operators : (int * int * assoc * idx node) list) : int =
  match operators with
  | [] -> failwith "find_split_position: no operators"
  | _ ->
      (* Find minimum precedence *)
      let min_prec = List.fold_left (fun acc (_, prec, _, _) -> min acc prec) max_int operators in

      (* Filter to operators with minimum precedence *)
      let min_prec_ops = List.filter (fun (_, prec, _, _) -> prec = min_prec) operators in

      (* Get associativity (they should all be the same for same precedence) *)
      let assoc = match min_prec_ops with
        | (_, _, a, _) :: _ -> a
        | [] -> failwith "impossible: empty min_prec_ops"
      in

      (* Pick position based on associativity *)
      match assoc with
      | Left ->
          (* Left-assoc: pick rightmost *)
          let (pos, _, _, _) = List.fold_left (fun acc curr ->
            let (p1, _, _, _) = acc in
            let (p2, _, _, _) = curr in
            if p2 > p1 then curr else acc
          ) (List.hd min_prec_ops) min_prec_ops in
          pos
      | Right ->
          (* Right-assoc: pick leftmost *)
          let (pos, _, _, _) = List.hd min_prec_ops in
          pos

(** Build left-associative function application from a sequence.
    f x y z becomes ((f x) y) z *)
let rec build_left_assoc_app (items : expression node list) : resolved_exp =
  match items with
  | [] -> failwith "build_left_assoc_app: empty sequence"
  | [single] -> ResolvedSingle single.value
  | f :: args -> ResolvedApp (ResolvedSingle f.value, args)

(** Take first n elements from a list *)
let rec take n lst =
  if n <= 0 then []
  else match lst with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs

(** Drop first n elements from a list *)
let rec drop n lst =
  if n <= 0 then lst
  else match lst with
    | [] -> []
    | _ :: xs -> drop (n - 1) xs

(** Resolve precedence in a flat expression sequence.

    Algorithm:
    1. Find all operators in the sequence
    2. If no operators: build left-associative application
    3. If operators exist:
       - Find lowest precedence operator (respecting associativity)
       - Split sequence at that operator
       - Recursively resolve left and right subsequences
       - Build infix application

    Example:
      [1; +; 2; times; 3]
      → Operators: [(1, 6, Left, +); (3, 7, Left, times)]
      → Lowest prec: 6 (+ at position 1)
      → Split: [1] + [2; times; 3]
      → Recurse: 1 + resolve([2; times; 3])
      → Result: 1 + (2 times 3) *)
let rec resolve_precedence (items : expression node list) : resolved_exp =
  match items with
  | [] -> failwith "resolve_precedence: empty sequence"
  | [single] -> ResolvedSingle single.value
  | items ->
      let operators = find_operators items in

      match operators with
      | [] ->
          (* No operators - pure function application *)
          build_left_assoc_app items
      | ops ->
          (* Find split position *)
          let split_pos = find_split_position ops in

          (* Extract operator and split sequence *)
          let left_items = take split_pos items in
          let op_exp = List.nth items split_pos in
          let right_items = drop (split_pos + 1) items in

          (* Extract operator identifier *)
          let op_idx = match extract_operator op_exp with
            | Some idx -> idx
            | None -> failwith "impossible: split position should be an operator"
          in

          (* Recursively resolve left and right *)
          let left_resolved = resolve_precedence left_items in
          let right_resolved = resolve_precedence right_items in

          (* Build infix application *)
          ResolvedInfix (left_resolved, op_idx, right_resolved)

(** Resolved pattern structure after precedence resolution. *)
type resolved_pat =
  | ResolvedPatSingle of pat
      (** Single pattern with no operators *)
  | ResolvedPatApp of resolved_pat * pat node list
      (** Left-associative pattern application *)
  | ResolvedPatInfix of resolved_pat * idx node * resolved_pat
      (** Binary infix pattern (e.g., x :: xs) *)

(** Extract operator from pattern node *)
let extract_pat_operator (p : pat node) : idx node option =
  match p.value with
  | PatIdx wo ->
      (match wo.value with
      | WithoutOp idx ->
          let name = Idx_utils.idx_to_string idx.value in
          if is_operator name then Some idx else None
      | WithOp idx ->
          (* op prefix - this is explicitly an operator being used as a value *)
          None)
  | _ -> None

(** Find operators in pattern sequence *)
let find_pat_operators (items : pat node list) : (int * int * assoc * idx node) list =
  let rec find_at pos = function
    | [] -> []
    | p :: rest ->
        (match extract_pat_operator p with
        | Some op_idx ->
            let op_name = Idx_utils.idx_to_string op_idx.value in
            (match get_precedence op_name with
            | Some (prec, assoc) ->
                (pos, prec, assoc, op_idx) :: find_at (pos + 1) rest
            | None ->
                find_at (pos + 1) rest)
        | None ->
            find_at (pos + 1) rest)
  in
  find_at 0 items

(** Build left-associative pattern application *)
let rec build_left_assoc_pat_app (items : pat node list) : resolved_pat =
  match items with
  | [] -> failwith "build_left_assoc_pat_app: empty sequence"
  | [single] -> ResolvedPatSingle single.value
  | f :: args -> ResolvedPatApp (ResolvedPatSingle f.value, args)

(** Resolve precedence in pattern sequences *)
let rec resolve_pat_precedence (items : pat node list) : resolved_pat =
  match items with
  | [] -> failwith "resolve_pat_precedence: empty sequence"
  | [single] -> ResolvedPatSingle single.value
  | items ->
      let operators = find_pat_operators items in

      match operators with
      | [] ->
          (* No operators - pure pattern application *)
          build_left_assoc_pat_app items
      | ops ->
          (* Find split position *)
          let split_pos = find_split_position ops in

          (* Extract operator and split sequence *)
          let left_items = take split_pos items in
          let op_pat = List.nth items split_pos in
          let right_items = drop (split_pos + 1) items in

          (* Extract operator identifier *)
          let op_idx = match extract_pat_operator op_pat with
            | Some idx -> idx
            | None -> failwith "impossible: split position should be an operator"
          in

          (* Recursively resolve left and right *)
          let left_resolved = resolve_pat_precedence left_items in
          let right_resolved = resolve_pat_precedence right_items in

          (* Build infix pattern *)
          ResolvedPatInfix (left_resolved, op_idx, right_resolved)
