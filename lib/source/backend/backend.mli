(** SML AST to OCaml Parsetree transformation.

    {1 Synopsis}

    This module implements the core transformation from SML abstract syntax trees
    to OCaml's compiler-libs Parsetree representation. It is the heart of the
    conversion process, handling all semantic differences between the two languages.

    {1 Overview}

    The backend is implemented as a functor parameterized over:
    - A {!Backend_sig.STORE} for name resolution and context
    - A {!Common.CONFIG} for conversion options and flags

    This allows the backend to be instantiated with different name resolution
    strategies and conversion policies.

    {1 Key Transformations}

    The backend handles several significant semantic transformations:

    {2 Type System Differences}
    - SML records → OCaml objects ([{x: int}] becomes [< x: int >])
    - SML tuples → OCaml tuples (semantically equivalent)
    - SML type variables → OCaml type variables (['a] remains ['a])

    {2 Name Transformations}
    - SML constructors → OCaml constructors ([SOME] → [Some])
    - SML module names → OCaml module names (capitalization)
    - Reserved word conflicts → Renamed identifiers ([type_] for [type])

    {2 Expression Transformations}
    - Pattern matching → OCaml match expressions
    - Record selectors ([#label]) → Anonymous functions ([fun r -> r#label])
    - Sequential expressions → OCaml sequences

    {2 Module System}
    - SML structures → OCaml modules
    - SML signatures → OCaml module types
    - SML functors → OCaml functors

    {1 Architecture}

    The transformation is implemented using:
    - Ppxlib's {!Ast_builder} for constructing OCaml AST nodes
    - Phantom locations ([Location.none]) for generated code
    - Name resolution via the {!Backend_sig.STORE} interface
    - Configuration-driven conversion policies

    {1 Usage Example}

    {[
      module MyStore = struct
        (* Implementation of STORE signature *)
      end

      module MyConfig = struct
        let config = {
          Common.input_file = "input.sml";
          output_file = Some "output.ml";
          verbosity = None;
          conversions = default_conversions;
        }
      end

      module Backend = Backend.Make(MyStore)(MyConfig)

      let sml_ast = (* ... parsed SML AST ... *) in
      let ocaml_ast = Backend.process_prog sml_ast
    ]}

    {1 Functor Parameters and Result} *)

(** [Make (Store) (Config)] creates a backend transformation module.

    This functor instantiates the complete SML-to-OCaml transformation
    pipeline with the specified name resolution store and configuration.

    @param Context Implementation of the {!Backend_sig.CONTEXT} signature,
                 providing name resolution and identifier transformation
                 capabilities.
    @param Config Implementation of the {!Common.CONFIG} signature,
                  providing conversion configuration including input/output
                  paths, verbosity settings, and conversion flags.

    @return A module implementing the {!Backend_sig.BACKEND} signature,
            which provides functions for transforming each SML AST node
            type to its OCaml equivalent. *)
module Make (Context : Backend_sig.CONTEXT) (Config : Common.CONFIG) :
  Backend_sig.BACKEND
