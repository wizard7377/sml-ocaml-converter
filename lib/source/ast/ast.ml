(** {1 SML Abstract Syntax Tree}

    Complete AST representation for Standard ML with support for SML/NJ
    Compilation Manager (CM) file descriptions.

    This is the main entry point for the AST library. It re-exports all
    types from sub-modules for backward compatibility with existing code.

    {2 Quick Start}

    {[
      open Ast

      (* Construct a simple expression: 42 *)
      let forty_two =
        box_node (ExpCon (box_node (ConInt (box_node "42"))))

      (* Pattern match on it *)
      match unbox_node forty_two with
      | ExpCon c -> Printf.printf "constant: %s\n" (show_constant (unbox_node c))
      | _ -> ()

      (* Build a function application: f x *)
      let app =
        box_node (ExpApp (
          box_node (ExpIdx (box_node (IdxIdx (box_node "f")))),
          box_node (ExpIdx (box_node (IdxIdx (box_node "x"))))
        ))
    ]}

    {2 Module Organization}

    The AST is split into focused modules for better organization:

    - {!Ast_node} - Generic node wrapper for all AST elements
      - Provides [box_node] and [unbox_node] utilities
      - Handles source position tracking
      - Enables metadata attachment

    - {!Ast_cm} - Compilation Manager (CM) file types
      - SML/NJ-specific build system representation
      - Library and group declarations
      - File dependency tracking

    - {!Ast_core} - Core SML AST (34 mutually recursive types)
      - Programs, declarations, expressions, patterns
      - Type system and bindings
      - Module system (structures, signatures, functors)

    {2 Backward Compatibility}

    All types and functions from the original monolithic [Ast] module
    are re-exported here. Existing code using [Ast.type_name] or [open Ast]
    will continue to work without modification:

    {[
      (* All these patterns work unchanged *)
      type t = Ast.expression          (* Qualified access *)
      let e : Ast.prog = ...           (* Type annotations *)

      open Ast                          (* Full import *)
      let x = box_node (ExpIdx ...)    (* Unqualified access *)
    ]}

    {2 Derived Operations}

    The AST provides [show] derivations for all types via [ppx_deriving.show]:

    {[
      let e = box_node (ExpCon (box_node (ConInt (box_node "42"))))
      Printf.printf "%s\n" (show_expression e)
      (* Prints: ExpCon {value = ConInt {value = "42"; pos = None}; pos = None} *)
    ]}
*)

include Ast_node
include Ast_cm
include Ast_core
