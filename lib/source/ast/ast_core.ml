(** {1 SML Abstract Syntax Tree - Core Language}

    Complete AST for Standard ML ('97 revision), representing all core language
    constructs and the module system (structures, signatures, functors).

    This module contains 34 mutually recursive types forming a single
    strongly-connected component. The mutual recursion is fundamental to SML's
    grammar and cannot be broken without architectural changes.

    {2 Type Organization}

    The AST types are organized into logical groups that mirror the structure of
    the SML Definition:

    {3 Top-Level Program Structure}
    - {!prog} - Top-level programs (sequences of declarations)
    - {!functor_binding} - Parameterized modules
    - {!signature_binding} - Named signature definitions

    {3 Core Language - Expressions and Patterns}
    - {!constant} - Literal constants (integers, strings, characters, etc.)
    - {!idx} - Identifiers (simple, qualified, type variables, labels)
    - {!expression} - Expressions (computation and control flow)
    - {!row} - Record expression rows
    - {!matching} - Pattern matching clauses
    - {!pat} - Patterns (destructuring and binding)
    - {!pat_row} - Record pattern rows
    - {!with_op} - Operator prefix handling

    {3 Core Language - Declarations and Bindings}
    - {!declaration} - Top-level and local declarations
    - {!fixity} - Operator fixity specifications
    - {!value_binding} - Value bindings (val declarations)
    - {!function_binding} - Function bindings (fun declarations)
    - {!fun_match} - Function pattern matching clauses
    - {!type_binding} - Type abbreviations
    - {!data_binding} - Datatype definitions
    - {!constructor_binding} - Data constructors
    - {!exn_bind} - Exception declarations

    {3 Module System - Structures and Signatures}
    - {!structure} - Module implementations
    - {!anotate} - Transparent vs opaque signature constraints
    - {!structure_binding} - Structure declarations
    - {!signature} - Module interfaces
    - {!typ_refine} - Type refinement (where clauses)
    - {!specification} - Signature specifications
    - {!val_specification} - Value specifications in signatures
    - {!typ_specification} - Type specifications in signatures
    - {!dat_specification} - Datatype specifications in signatures
    - {!con_specification} - Constructor specifications
    - {!exn_specification} - Exception specifications
    - {!str_specification} - Structure specifications

    {3 Type System}
    - {!typ} - Type expressions
    - {!typ_row} - Record type rows

    {2 Mutual Recursion Rationale}

    The 34 types in this module form an unavoidable dependency cycle:

    {3 Core Circular Dependencies}
    - [expression ↔ matching ↔ pat]: Expressions contain patterns
      (case/fn/handle), patterns contain expressions (as-patterns), and match
      clauses connect both
    - [declaration → all binding types]: Declarations reference value_binding,
      function_binding, type_binding, etc.
    - [pat ↔ pat_row], [typ ↔ typ_row]: Records require their row types
    - [structure ↔ declaration]: Structures contain declarations
    - [specification ↔ signature]: Signatures contain specifications

    This mirrors Standard ML's inherently recursive grammar and is not a design
    limitation but a faithful representation of the language structure.

    {2 Grammar Source}

    This AST follows the grammar from the Definition of Standard ML (Revised
    1997), including derived forms from Appendix A.

    @see <https://smlfamily.github.io/sml97-defn.pdf> SML '97 Definition (PDF)
    @see <http://sml-family.org/> Standard ML Family *)

open Ast_node

type prog =
  | ProgDec of declaration node
      (** Core declaration at the top level.

          Example: [val x = 42] or [structure S = struct end]
          {[
            ProgDec (box_node (ValDec ([], box_node (ValBind (...)))))
          ]} *)
  | ProgFun of functor_binding node
      (** Functor declaration: [functor fctbind].

          Example: [functor F(X : S) = struct end]
          @see 'functor_binding' Functor binding details *)
  | ProgStr of signature_binding node
      (** Signature declaration: [signature sigbind].

          Example: [signature S = sig type t end]
          @see 'signature_binding' Signature binding details *)
  | ProgSeq of prog node * prog node
      (** Sequence of programs: [prog1 ; prog2].

          Programs are evaluated left-to-right. The semicolon between top-level
          declarations is optional in SML.
          {[
            (* val x = 1; val y = 2 *)
            ProgSeq (
              box_node (ProgDec (box_node (ValDec ...))),
              box_node (ProgDec (box_node (ValDec ...)))
            )
          ]} *)
  | ProgEmpty
      (** Empty program.

          Represents an empty source file or the base case when building program
          sequences. *)
[@@deriving show]

(** Functor bindings.

    {[
      fctbind ::= id1 ( id2 : sig ) [:[:>] sig] = structure [and fctbind]    (* plain *)
                | id ( specification ) [:[:>] sig] = structure [and fctbind]          (* opened *)
    ]}

    Functors are parameterized modules that take a structure matching a
    signature and produce a new structure. They enable generic programming over
    module structures.

    {3 Example: Plain Functor}
    SML: [functor MkSet(Ord : ORDERED) : SET = struct ... end]
    {[
      FctBind (
        box_node (IdxIdx (box_node "MkSet")),       (* functor name *)
        box_node (IdxIdx (box_node "Ord")),         (* parameter name *)
        box_node (SignIdx (box_node (IdxIdx ...))), (* ORDERED *)
        Some (box_node Transparent, box_node (SignIdx ...)), (* : SET *)
        box_node (StructStr ...),                   (* body *)
        None                                        (* no more bindings *)
      )
    ]}

    @see 'signature' Signature types for parameter and result constraints
    @see 'structure' Structure expressions for functor bodies *)
and functor_binding =
  | FctGen of
      idx node
      * (anotate node * signature node) option
      * structure node
      * functor_binding node option
      (** Generative functor (SML/NJ extension): [id [:[\:>] sig] = structure].

          A simpler functor form without explicit parameter specification. The
          functor name is followed directly by an optional result signature and
          the body structure expression.

          This is a language extension provided by SML/NJ that allows defining
          functors without naming the parameter structure.

          Components:
          - Functor name
          - Optional result annotation (transparent [:] or opaque [:>])
          - Body structure expression
          - Optional additional bindings ([and ...])

          {[
            (* SML/NJ: functor F : SIG = struct ... end *)
            FctGen
              ( box_node (IdxIdx (box_node "F")),
                Some (box_node Transparent, box_node (SignIdx sig_id)),
                box_node (StructStr body_dec),
                None )
          ]}

          @see 'FctBind' Standard SML functor with explicit parameters *)
  | FctBind of
      idx node
      * idx node
      * signature node
      * (anotate node * signature node) option
      * structure node
      * functor_binding node option
      (** Plain functor: [id1 ( id2 : sig ) [:[:>] sig] = structure].

          Components:
          - Functor name
          - Parameter name
          - Parameter signature constraint
          - Optional result annotation (transparent [:] or opaque [:>])
          - Body structure expression
          - Optional additional bindings ([and ...])

          @see 'anotate' Transparent vs opaque sealing *)
  | FctBindOpen of
      idx node
      * specification node
      * (anotate node * signature node) option
      * structure node
      * functor_binding node option
      (** Opened functor: [id ( specification ) [:[:>] sig] = structure].

          The specification components are directly visible in the functor body
          without requiring qualification through a parameter name. This is
          syntactic sugar for a functor with an anonymous structure parameter.

          Example: [functor F(type t val x : t) = struct ... end] The [t] and
          [x] are directly accessible in the body. *)

(** Signature bindings.

    {[
      sigbind ::= id = sig [and sigbind]
    ]}

    Binds identifiers to signatures for later use in structure annotations and
    functor parameter constraints.

    {3 Example}
    SML: [signature ORDERED = sig type t val compare : t * t -> int end]
    {[
      SignBind (
        box_node (IdxIdx (box_node "ORDERED")),
        box_node (SignSig (...)),
        None
      )
    ]} *)
and signature_binding =
  | SignBind of idx node * signature node * signature_binding node option
      (** Signature binding: [id = sig].

          Components:
          - Signature name (conventionally UPPER_CASE)
          - Signature expression
          - Optional additional bindings ([and ...]) *)

(** {1 Core Language}

    The core language includes constants, identifiers, expressions, patterns,
    types, and declarations. These form the computational heart of SML programs.

    The core language is stratified into:
    - {!constant} - Literal constants (integers, strings, etc.)
    - {!idx} - Identifiers and qualified names
    - {!expression} - Expressions (computation)
    - {!pat} - Patterns (destructuring)
    - {!typ} - Type expressions
    - {!declaration} - Declarations (bindings) *)

(** {2 Constants}

    {[
      constant ::= int       (* integer *)
            | word      (* word *)
            | float     (* floating point *)
            | char      (* character *)
            | string    (* string *)
    ]}

    Integer constants may be decimal or hexadecimal (prefixed with [0x]), and
    may have an optional negation prefix [~]. Word constants are unsigned and
    prefixed with [0w] (decimal) or [0wx] (hex). Character constants are written
    as [#"c"]. String constants are enclosed in double quotes.

    {3 Construction Examples}
    {[
      (* Integer: 42 *)
      let int_42 = box_node (ConInt (box_node "42"))

      (* Negative integer: ~5 *)
      let neg_5 = box_node (ConInt (box_node "~5"))

      (* Hexadecimal: 0xFF *)
      let hex_ff = box_node (ConInt (box_node "0xFF"))

      (* Word: 0w42 *)
      let word_42 = box_node (ConWord (box_node "0w42"))

      (* Float: 3.14 *)
      let pi = box_node (ConFloat (box_node "3.14"))

      (* Character: #"a" *)
      let char_a = box_node (ConChar (box_node "a"))

      (* String: "hello" *)
      let hello = box_node (ConString (box_node "hello"))
    ]} *)
and constant =
  | ConInt of string node
      (** Integer constant.

          Decimal: [[~]num], hexadecimal: [[~]0xhex]. The string preserves the
          original source representation.

          Examples: ["42"], ["~100"], ["0xFF"], ["~0x1A"] *)
  | ConWord of string node
      (** Word (unsigned integer) constant.

          Decimal: [0wnum], hexadecimal: [0wxhex]. Words are used for bit
          manipulation and unsigned arithmetic.

          Examples: ["0w255"], ["0wx1F"] *)
  | ConFloat of string node
      (** Floating point constant.

          Format: [[~]num.num] or [[~]num[.num]e[~]num]. Supports scientific
          notation.

          Examples: ["3.14"], ["~2.0"], ["1.0e10"], ["6.022e~23"] *)
  | ConChar of string node
      (** Character constant.

          SML syntax: [#"ascii"]. The stored string contains just the character
          without the [#"..."] delimiters.

          Examples: ["a"], ["\\n"], ["\\t"] *)
  | ConString of string node
      (** String constant.

          SML syntax: ["ascii*"]. The stored string contains the content without
          surrounding quotes, with escapes preserved.

          Examples: ["hello"], ["line1\\nline2"] *)
  
(** {2 Identifiers}

    {[
      id     ::= letter (letter | digit | ' | _)*     (* alphanumeric *)
               | (! | % | & | $ | # | + | - | ...)+   (* symbolic *)
      var    ::= 'letter (letter | digit | ' | _)*    (* unconstrained type variable *)
               | ''letter (letter | digit | ' | _)*   (* equality type variable *)
      longid ::= id1.....idn                          (* qualified, n >= 1 *)
      lab    ::= id                                   (* identifier label *)
               | num                                  (* numeric label, may not start with 0 *)
    ]}

    Identifiers can be alphanumeric or symbolic. Type variables are prefixed
    with a single quote ([']) or double quote (['']) for equality type
    variables. Long identifiers are dot-separated qualified names.

    {3 Construction Examples}
    {[
      (* Simple identifier: x *)
      let x = box_node (IdxIdx (box_node "x"))

      (* Symbolic identifier: ++ *)
      let plus_plus = box_node (IdxIdx (box_node "++"))

      (* Type variable: 'a *)
      let alpha = box_node (IdxVar (box_node "a"))

      (* Equality type variable: ''a *)
      let eq_alpha = box_node (IdxVar (box_node "'a"))

      (* Long identifier: List.map *)
      let list_map =
        box_node
          (IdxLong
             [
               box_node (IdxIdx (box_node "List"));
               box_node (IdxIdx (box_node "map"));
             ])

      (* Numeric label for tuples: #1 *)
      let label_1 = box_node (IdxNum (box_node "1"))
    ]}

    @see 'expression' Expressions that use identifiers
    @see 'pat' Patterns that bind identifiers *)
and idx =
  | IdxIdx of string node
      (** Simple alphanumeric or symbolic identifier.

          Alphanumeric: starts with letter, contains letters/digits/[_]/['].
          Symbolic: sequence of [!%&$#+-/:<=>?@\\~'^|*].

          Examples: ["x"], ["myVar"], ["++"], ["<>"], ["::"] *)
  | IdxVar of string node
      (** Type variable: ['var] or [''var] for equality type variables.

          The stored string excludes the leading quote(s). Single quote ['] =
          unconstrained, double [''] = admits equality.

          Examples: ["a"] for ['a], ["'a"] for [''a] *)
  | IdxLong of idx node list
      (** Long (qualified) identifier: [id1.id2...idn].

          Used for accessing module members. Each component is typically an
          {!IdxIdx}. The list has at least one element.

          Example: [List.map] -> [[IdxIdx "List"; IdxIdx "map"]] *)
  | IdxLab of string node
      (** Record label (alphanumeric identifier).

          Used in record expressions and patterns. Syntactically identical to
          regular identifiers but in label context.

          Example: In [{x = 1, y = 2}], ["x"] and ["y"] are labels. *)
  | IdxNum of string node
      (** Numeric label for tuple access (1-indexed, may not start with 0).

          Used with the [#] selector for tuple component access. SML tuples are
          sugar for records with numeric labels.

          Example: [#2 (1, "a", true)] accesses the second element. *)

(** {2 Expressions}

    {[
      expression ::= constant                                    (* constant *)
            | [op] longid                            (* value or constructor identifier *)
            | exp1 exp2                              (* application *)
            | exp1 id exp2                           (* infix application *)
            | ( expression )                                (* parentheses *)
            | ( exp1 , ... , expn )                  (* tuple, n != 1 *)
            | { [exprow] }                           (* record *)
            | # lab                                  (* record selector *)
            | [ exp1 , ... , expn ]                  (* list, n >= 0 *)
            | ( exp1 ; ... ; expn )                  (* sequence, n >= 2 *)
            | let declaration in exp1 ; ... ; expn end       (* local declaration, n >= 1 *)
            | expression : typ                              (* type annotation *)
            | raise expression                              (* exception raising *)
            | expression handle match                       (* exception handling *)
            | exp1 andalso exp2                      (* conjunction *)
            | exp1 orelse exp2                       (* disjunction *)
            | if exp1 then exp2 else exp3            (* conditional *)
            | while exp1 do exp2                     (* iteration *)
            | case expression of match                      (* case analysis *)
            | fn match                               (* function *)
    ]}

    Expressions are the computational core of SML. They evaluate to values and
    can have side effects (via references, I/O, or exceptions).

    @see 'pat' Patterns used in function arguments and case branches
    @see 'matching' Match clauses for case/handle/fn
    @see 'declaration' Declarations used in let expressions *)
and expression =
  | ExpCon of constant node
      (** Constant expression - literal values.

          Represents literal constant values that appear directly in source code.
          Constants are self-evaluating: they evaluate to themselves and have
          no side effects.

          {3 SML Syntax}

          {v
            expression ::= constant    (* integer, word, float, char, string *)
          v}

          {3 Examples}

          {[
            (* SML: 42 *)
            ExpCon (box_node (ConInt (box_node "42")))

            (* SML: "hello" *)
            ExpCon (box_node (ConString (box_node "hello")))

            (* SML: #"x" *)
            ExpCon (box_node (ConChar (box_node "x")))

            (* SML: 0w255 *)
            ExpCon (box_node (ConWord (box_node "0w255")))

            (* SML: 3.14159 *)
            ExpCon (box_node (ConFloat (box_node "3.14159")))
          ]}

          {3 Semantics}

          - Constants always evaluate to themselves
          - Type is determined by the constant format:
            - Integer literals: [int] type
            - String literals: [string] type
            - Character literals: [char] type
            - Word literals: [word] type
            - Float literals: [real] type
          - No side effects, no exceptions
          - Constants are immutable values

          {3 Type Inference}

          Integer and float literals can be overloaded - their exact type
          may depend on context:
          {[
            (* SML: fun f (x : Int32.int) = x + 1 *)
            (* The "1" has type Int32.int, not default int *)
          ]}

          @see constant All constant types
          @see ExpIdx Variable and constructor references *)
  | ExpIdx of idx node
      (** Identifier expression - variable or constructor reference.

          References a value, function, or data constructor by name. The identifier
          may be simple (unqualified) or qualified with module path. The [op] prefix
          allows using infix operators in prefix position.

          {3 SML Syntax}

          {v
            expression ::= [op] longid    (* value, function, or constructor *)
          v}

          {3 Examples}

          {[
            (* SML: x  -- simple variable *)
            ExpIdx (box_node (IdxIdx (box_node "x")))

            (* SML: List.map  -- qualified function *)
            ExpIdx (box_node (IdxLong [
              box_node (IdxIdx (box_node "List"));
              box_node (IdxIdx (box_node "map"))
            ]))

            (* SML: SOME  -- nullary constructor *)
            ExpIdx (box_node (IdxIdx (box_node "SOME")))

            (* SML: op +  -- infix operator as prefix *)
            ExpIdx (box_node (IdxIdx (box_node "+")))

            (* SML: MyModule.Inner.value  -- nested module access *)
            ExpIdx (box_node (IdxLong [
              box_node (IdxIdx (box_node "MyModule"));
              box_node (IdxIdx (box_node "Inner"));
              box_node (IdxIdx (box_node "value"))
            ]))
          ]}

          {3 Semantics}

          - Evaluates to the value bound to the identifier in scope
          - Raises exception if identifier is unbound
          - Constructors evaluate to constructor functions
          - Must be in scope at use site (via val, fun, datatype, or open)

          {3 Operator Prefix}

          The [op] keyword removes infix status, allowing operators to be
          used as ordinary functions:
          {[
            (* SML: op + (1, 2)  -- using + as a function *)
            ExpApp (
              box_node (ExpIdx (box_node (IdxIdx (box_node "+")))),
              box_node (TupleExp [one; two])
            )

            (* SML: map (op ^) pairs  -- passing operator to higher-order fn *)
            ExpApp (
              box_node (ExpIdx (box_node (IdxIdx (box_node "map")))),
              box_node (ExpIdx (box_node (IdxIdx (box_node "^"))))
            )
          ]}

          @see idx Identifier types
          @see ExpApp Function application
          @see InfixApp Infix operator usage *)
  | ExpApp of expression node * expression node
      (** Function application - applying a function to an argument.

          Applies the function (first expression) to the argument (second expression).
          SML uses left-associative application, so [f x y] parses as [(f x) y].
          Multi-argument functions are either curried or use tuple arguments.

          {3 SML Syntax}

          {v
            expression ::= exp1 exp2    (* left-associative function application *)
          v}

          {3 Examples}

          {[
            (* SML: f x  -- simple application *)
            ExpApp (
              box_node (ExpIdx (box_node (IdxIdx (box_node "f")))),
              box_node (ExpIdx (box_node (IdxIdx (box_node "x"))))
            )

            (* SML: f x y  -- curried application: (f x) y *)
            ExpApp (
              box_node (ExpApp (
                box_node (ExpIdx (box_node (IdxIdx (box_node "f")))),
                box_node (ExpIdx (box_node (IdxIdx (box_node "x"))))
              )),
              box_node (ExpIdx (box_node (IdxIdx (box_node "y"))))
            )

            (* SML: SOME 42  -- constructor application *)
            ExpApp (
              box_node (ExpIdx (box_node (IdxIdx (box_node "SOME")))),
              box_node (ExpCon (box_node (ConInt (box_node "42"))))
            )

            (* SML: (fn x => x + 1) 5  -- applying anonymous function *)
            ExpApp (
              box_node (FnExp (box_node (Case (x_pat, x_plus_1, None)))),
              box_node (ExpCon (box_node (ConInt (box_node "5"))))
            )

            (* SML: length [1,2,3]  -- list argument *)
            ExpApp (
              box_node (ExpIdx (box_node (IdxIdx (box_node "length")))),
              box_node (ListExp [one; two; three])
            )
          ]}

          {3 Semantics}

          - Left expression must evaluate to a function value
          - Right expression (argument) is evaluated first
          - Then function is applied to the argument value
          - Raises exception if left side is not a function
          - Order of evaluation: argument before function application

          {3 Associativity}

          Application is {b left-associative}:
          {[
            (* SML: f x y z  parses as  ((f x) y) z *)
            ExpApp (
              ExpApp (
                ExpApp (f, x),
                y
              ),
              z
            )
          ]}

          This enables currying - partial application of multi-argument functions.

          @see FnExp Anonymous functions
          @see InfixApp Infix operator application
          @see ExpIdx Variable and constructor references *)
  | InfixApp of expression node * idx node * expression node
      (** Infix operator application - binary operator usage.

          Applies an infix operator to two operand expressions. The operator
          must be declared [infix] or [infixr] with a precedence (0-9).
          The parser resolves precedence and associativity to build the correct
          AST structure.

          {3 SML Syntax}

          {v
            expression ::= exp1 id exp2    (* id must be declared infix *)
          v}

          {3 Examples}

          {[
            (* SML: 1 + 2  -- arithmetic operator *)
            InfixApp (
              box_node (ExpCon (box_node (ConInt (box_node "1")))),
              box_node (IdxIdx (box_node "+")),
              box_node (ExpCon (box_node (ConInt (box_node "2"))))
            )

            (* SML: x :: xs  -- cons operator *)
            InfixApp (
              box_node (ExpIdx (box_node (IdxIdx (box_node "x")))),
              box_node (IdxIdx (box_node "::")),
              box_node (ExpIdx (box_node (IdxIdx (box_node "xs"))))
            )

            (* SML: a andalso b  -- boolean operator *)
            (* Note: andalso/orelse have dedicated constructors *)

            (* SML: name ^ " world"  -- string concatenation *)
            InfixApp (
              box_node (ExpIdx (box_node (IdxIdx (box_node "name")))),
              box_node (IdxIdx (box_node "^")),
              box_node (ExpCon (box_node (ConString (box_node " world"))))
            )

            (* SML: 1 + 2 * 3  -- precedence: parses as 1 + (2 * 3) *)
            InfixApp (
              box_node (ExpCon (box_node (ConInt (box_node "1")))),
              box_node (IdxIdx (box_node "+")),
              box_node (InfixApp (two, times, three))
            )
          ]}

          {3 Semantics}

          - Both operands are evaluated (order unspecified in SML)
          - Operator is applied to the two values
          - Operator must be in scope and declared infix
          - Actual function may be user-defined or built-in

          {3 Precedence and Associativity}

          Standard SML operator precedence (higher binds tighter):
          - Precedence 7: [*], [/], [div], [mod]
          - Precedence 6: [+], [-], [^]
          - Precedence 5: [::], [@]
          - Precedence 4: [=], [<>], [<], [>], [<=], [>=]
          - Precedence 3: [:=], [o]
          - Precedence 0: [before]

          Left-associative ([infix]): [a + b + c] = [(a + b) + c]
          Right-associative ([infixr]): [a :: b :: c] = [a :: (b :: c)]

          {3 Custom Operators}

          Users can define custom infix operators:
          {[
            (* SML:
               infix 6 +++
               fun x +++ y = x + y + 1
               val z = 1 +++ 2 +++ 3
            *)
            InfixApp (
              box_node (InfixApp (one, plusplus, two)),
              box_node (IdxIdx (box_node "+++")),
              three
            )
          ]}

          @see FixityDec Fixity declarations
          @see fixity Operator precedence
          @see ExpApp Prefix function application *)
  | ParenExp of expression node
      (** Parenthesized expression - explicit grouping.

          Represents an expression wrapped in parentheses for grouping or
          clarity. Semantically equivalent to the inner expression, but preserved
          in the AST to maintain source structure for pretty-printing and
          error messages.

          {3 SML Syntax}

          {v
            expression ::= ( expression )    (* explicit grouping *)
          v}

          {3 Examples}

          {[
            (* SML: (x)  -- redundant parentheses *)
            ParenExp (box_node (ExpIdx (box_node (IdxIdx (box_node "x")))))

            (* SML: (1 + 2) * 3  -- override precedence *)
            ExpApp (
              box_node (ParenExp (
                box_node (InfixApp (one, plus, two))
              )),
              three
            )

            (* SML: (fn x => x + 1)  -- clarify function boundaries *)
            ParenExp (box_node (FnExp (box_node (Case (x_pat, x_plus_1, None)))))

            (* SML: ((nested))  -- multiple levels *)
            ParenExp (box_node (ParenExp (box_node (ExpIdx x_id))))
          ]}

          {3 Semantics}

          - Evaluates to the same value as the inner expression
          - No runtime overhead (purely syntactic)
          - Used for:
            - Overriding operator precedence: [(a + b) * c]
            - Clarifying complex expressions
            - Aesthetic preferences in source code

          {3 Distinction from Tuples}

          Important: [( expr )] with one element is NOT a tuple:
          {[
            (* SML: (42)  -- parenthesized int, not tuple *)
            ParenExp (box_node (ExpCon (box_node (ConInt (box_node "42")))))

            (* SML: (42,)  -- syntax error in SML (unlike Python/Haskell) *)

            (* SML: (42, 43)  -- this IS a tuple *)
            TupleExp [forty_two; forty_three]
          ]}

          The parser distinguishes based on whether a comma appears:
          - [(expr)] → ParenExp
          - [(expr1, expr2, ...)] → TupleExp

          @see TupleExp Tuple expressions (2+ elements)
          @see InfixApp Operator precedence *)
  | TupleExp of expression node list
      (** Tuple expression - ordered collection of heterogeneous values.

          Represents a tuple of zero or more expressions. The unit value [()] is
          a 0-tuple (empty list), 2-tuples are pairs, 3-tuples are triples, etc.
          Single-element "tuples" don't exist - use ParenExp instead.

          {3 SML Syntax}

          {v
            expression ::= ( )                      (* unit *)
                         | ( exp1 , ... , expn )    (* tuple, n >= 2 *)
          v}

          {3 Examples}

          {[
            (* SML: ()  -- unit value *)
            TupleExp []

            (* SML: (1, 2)  -- pair *)
            TupleExp [
              box_node (ExpCon (box_node (ConInt (box_node "1"))));
              box_node (ExpCon (box_node (ConInt (box_node "2"))))
            ]

            (* SML: (1, "a", true)  -- triple with different types *)
            TupleExp [
              box_node (ExpCon (box_node (ConInt (box_node "1"))));
              box_node (ExpCon (box_node (ConString (box_node "a"))));
              box_node (ExpIdx (box_node (IdxIdx (box_node "true"))))
            ]

            (* SML: (x, y, z, w)  -- 4-tuple *)
            TupleExp [x_node; y_node; z_node; w_node]

            (* SML: ((1, 2), (3, 4))  -- nested tuples *)
            TupleExp [
              box_node (TupleExp [one; two]);
              box_node (TupleExp [three; four])
            ]
          ]}

          {3 Semantics}

          - All component expressions are evaluated (order unspecified)
          - Results are packaged into a tuple value
          - Type is product type: [t1 * t2 * ... * tn]
          - Unit [()] has type [unit]
          - Elements can have different types (heterogeneous)

          {3 Syntactic Sugar}

          Tuples are actually syntactic sugar for records with numeric labels:
          {[
            (* SML: (1, 2, 3)  desugars to  {1=1, 2=2, 3=3} *)
            (* This is why #1, #2, #3 work as selectors *)
          ]}

          However, the AST represents them as TupleExp for convenience.

          {3 Pattern Matching}

          Tuples can be destructured with pattern matching:
          {[
            (* SML: case (1, 2) of (x, y) => x + y *)
            CaseExp (
              box_node (TupleExp [one; two]),
              box_node (Case (
                box_node (PatTuple [x_pat; y_pat]),
                box_node (InfixApp (x, plus, y)),
                None
              ))
            )
          ]}

          @see PatTuple Tuple patterns
          @see ParenExp Single-element parentheses
          @see RecordExp Named record fields *)
  | RecordExp of row node list
      (** Record expression - named collection of labeled fields.

          Represents a record with labeled fields. Unlike tuples, records use
          explicit field names for access. The empty record [{}] is equivalent
          to the unit value [()].

          {3 SML Syntax}

          {v
            expression ::= { }                          (* empty record = unit *)
                         | { lab1 = exp1 , ... , labn = expn }    (* n >= 1 *)
          v}

          {3 Examples}

          {[
            (* SML: {}  -- empty record (equivalent to unit) *)
            RecordExp []

            (* SML: {x = 1, y = 2}  -- simple 2D point *)
            RecordExp [
              box_node (Row (
                box_node (IdxLab (box_node "x")),
                box_node (ExpCon (box_node (ConInt (box_node "1")))),
                None
              ));
              box_node (Row (
                box_node (IdxLab (box_node "y")),
                box_node (ExpCon (box_node (ConInt (box_node "2")))),
                None
              ))
            ]

            (* SML: {name = "Alice", age = 30, active = true} *)
            RecordExp [
              box_node (Row (
                box_node (IdxLab (box_node "name")),
                box_node (ExpCon (box_node (ConString (box_node "Alice")))),
                None
              ));
              box_node (Row (
                box_node (IdxLab (box_node "age")),
                box_node (ExpCon (box_node (ConInt (box_node "30")))),
                None
              ));
              box_node (Row (
                box_node (IdxLab (box_node "active")),
                box_node (ExpIdx (box_node (IdxIdx (box_node "true")))),
                None
              ))
            ]

            (* SML: {x = y + 1}  -- field can be arbitrary expression *)
            RecordExp [
              box_node (Row (
                box_node (IdxLab (box_node "x")),
                box_node (InfixApp (y_node, plus_op, int_1)),
                None
              ))
            ]

            (* SML: {pos = {x = 0, y = 0}, velocity = 5}  -- nested records *)
            RecordExp [
              box_node (Row (
                box_node (IdxLab (box_node "pos")),
                box_node (RecordExp [x_row; y_row]),
                None
              ));
              box_node (Row (
                box_node (IdxLab (box_node "velocity")),
                int_5,
                None
              ))
            ]
          ]}

          {3 Semantics}

          - All field expressions are evaluated (order unspecified)
          - Field labels must be unique within a record
          - Record type is [{lab1: t1, lab2: t2, ..., labn: tn}]
          - Empty record [{}] has type [unit]
          - Fields can have different types (heterogeneous)

          {3 Field Access}

          Records are accessed using [#label] selector syntax:
          {[
            (* SML: #x {x = 1, y = 2}  evaluates to  1 *)
            ExpApp (
              box_node (RecordSelector (box_node (IdxLab (box_node "x")))),
              box_node (RecordExp [x_row; y_row])
            )
          ]}

          {3 Relation to Tuples}

          Tuples are syntactic sugar for records with numeric labels:
          {[
            (* SML: (1, 2, 3)  desugars to  {1=1, 2=2, 3=3} *)
            (* That's why #1, #2, #3 work on tuples *)
          ]}

          @see RecordSelector Field selection syntax
          @see PatRecord Record patterns
          @see TupleExp Tuple expressions (numeric labels)
          @see row Row structure for field bindings *)
  | RecordSelector of idx node
      (** Record field selector - first-class field extraction function.

          Creates a first-class function that extracts a specific field from a
          record. The [#label] syntax is syntactic sugar for an anonymous function
          that pattern matches on a record and returns the labeled field.

          {3 SML Syntax}

          {v
            expression ::= # lab        (* field selector *)
          v}

          {3 Desugaring}

          The selector [#lab] desugars to:
          {v
            fn {lab = x, ...} => x
          v}

          This creates a polymorphic function that works on any record containing
          the specified label.

          {3 Examples}

          {[
            (* SML: #name  -- extract 'name' field *)
            RecordSelector (box_node (IdxLab (box_node "name")))

            (* Usage: #name {name = "Alice", age = 30}  evaluates to  "Alice" *)
            ExpApp (
              box_node (RecordSelector (box_node (IdxLab (box_node "name")))),
              box_node (RecordExp [name_row; age_row])
            )

            (* SML: #x  -- extract 'x' coordinate *)
            RecordSelector (box_node (IdxLab (box_node "x")))

            (* SML: #1  -- tuple first element selector *)
            RecordSelector (box_node (IdxNum (box_node "1")))

            (* Usage: #1 (10, 20)  evaluates to  10 *)
            ExpApp (
              box_node (RecordSelector (box_node (IdxNum (box_node "1")))),
              box_node (TupleExp [int_10; int_20])
            )

            (* SML: map #age people  -- extract ages from person records *)
            ExpApp (
              box_node (ExpApp (
                map_fn,
                box_node (RecordSelector (box_node (IdxLab (box_node "age"))))
              )),
              people_list
            )
          ]}

          {3 Semantics}

          - Creates a function value of type [{..., lab: t, ...} -> t]
          - Polymorphic in the other fields (row polymorphism)
          - Can be passed to higher-order functions (map, filter, etc.)
          - Type is inferred from usage context

          {3 Tuple Access}

          Since tuples are syntactic sugar for numeric-labeled records, selectors
          work on tuples too:
          {[
            (* SML: #1 (a, b)  -->  a *)
            (* SML: #2 (a, b)  -->  b *)
            (* SML: #3 (a, b, c)  -->  c *)
          ]}

          {3 Type Inference}

          The selector's type is inferred from usage:
          {[
            (* SML: val getName = #name *)
            (* Type: {name: 'a, ...} -> 'a *)

            (* SML: getName {name = "Bob", age = 25}  -- 'a = string *)
            (* Type becomes: {name: string, ...} -> string *)
          ]}

          @see RecordExp Record construction
          @see PatRecord Record pattern matching
          @see TupleExp Tuple expressions
          @see ExpApp Function application *)
  | ArrayExp of expression node list
  | ListExp of expression node list
      (** List expression - homogeneous sequential collection.

          Represents a list literal using bracket notation. List expressions are
          syntactic sugar for nested applications of the cons operator [::] and
          the nil constructor [[]]. All elements must have the same type.

          {3 SML Syntax}

          {v
            expression ::= [ ]                          (* empty list *)
                         | [ exp1 , ... , expn ]        (* list literal, n >= 1 *)
          v}

          {3 Desugaring}

          List literals desugar to cons applications:
          {v
            []        =  nil
            [1]       =  1 :: nil
            [1, 2, 3] =  1 :: 2 :: 3 :: nil
          v}

          The AST represents this as ListExp for convenience, but semantically
          it's equivalent to the desugared form.

          {3 Examples}

          {[
            (* SML: []  -- empty list *)
            ListExp []

            (* SML: [1]  -- singleton list *)
            ListExp [box_node (ExpCon (box_node (ConInt (box_node "1"))))]

            (* SML: [1, 2, 3]  -- integer list *)
            ListExp [
              box_node (ExpCon (box_node (ConInt (box_node "1"))));
              box_node (ExpCon (box_node (ConInt (box_node "2"))));
              box_node (ExpCon (box_node (ConInt (box_node "3"))))
            ]

            (* SML: ["a", "b", "c"]  -- string list *)
            ListExp [
              box_node (ExpCon (box_node (ConString (box_node "a"))));
              box_node (ExpCon (box_node (ConString (box_node "b"))));
              box_node (ExpCon (box_node (ConString (box_node "c"))))
            ]

            (* SML: [x + 1, y * 2]  -- expressions as elements *)
            ListExp [
              box_node (InfixApp (x, plus, int_1));
              box_node (InfixApp (y, times, int_2))
            ]

            (* SML: [[1, 2], [3, 4]]  -- nested lists *)
            ListExp [
              box_node (ListExp [int_1; int_2]);
              box_node (ListExp [int_3; int_4])
            ]
          ]}

          {3 Semantics}

          - All element expressions are evaluated (order unspecified)
          - Results are linked into a cons cell chain
          - Type is ['a list] where all elements have type ['a]
          - Empty list [[]] has polymorphic type ['a list]
          - Homogeneous: all elements must have the same type

          {3 Relation to Cons Operator}

          The following are equivalent:
          {[
            (* SML: [1, 2, 3] *)
            ListExp [int_1; int_2; int_3]

            (* SML: 1 :: 2 :: 3 :: [] *)
            InfixApp (
              int_1,
              box_node (IdxIdx (box_node "::")),
              box_node (InfixApp (
                int_2,
                box_node (IdxIdx (box_node "::")),
                box_node (InfixApp (
                  int_3,
                  box_node (IdxIdx (box_node "::")),
                  box_node (ListExp [])
                ))
              ))
            )
          ]}

          {3 Pattern Matching}

          Lists are commonly destructured using patterns:
          {[
            (* SML: case [1, 2] of [] => 0 | x::xs => x *)
            CaseExp (
              box_node (ListExp [int_1; int_2]),
              box_node (Case (
                box_node (PatIdx (box_node (WithoutOp (nil_id)))),
                int_0,
                Some (box_node (Case (
                  box_node (PatInfix (x_pat, cons_op, xs_pat)),
                  x_exp,
                  None
                )))
              ))
            )
          ]}

          @see InfixApp Cons operator [::] application
          @see PatList List patterns
          @see PatInfix Cons pattern [x::xs] *)
  | SeqExp of expression node list
      (** Sequential expression - imperative sequencing with semicolons.

          Evaluates multiple expressions in left-to-right order, discarding all
          results except the last. Earlier expressions are evaluated purely for
          their side effects (I/O, mutation, exceptions). The sequence must
          contain at least 2 expressions.

          {3 SML Syntax}

          {v
            expression ::= ( exp1 ; exp2 ; ... ; expn )    (* n >= 2 *)
          v}

          {3 Examples}

          {[
            (* SML: (print "Starting..."; 42)  -- side effect then value *)
            SeqExp [
              box_node (ExpApp (print_fn, box_node (ExpCon (box_node (ConString (box_node "Starting..."))))));
              box_node (ExpCon (box_node (ConInt (box_node "42"))))
            ]

            (* SML: (x := 10; y := 20; !x + !y)  -- multiple assignments *)
            SeqExp [
              box_node (ExpApp (assign_x, int_10));
              box_node (ExpApp (assign_y, int_20));
              box_node (InfixApp (deref_x, plus, deref_y))
            ]

            (* SML: (print "a"; print "b"; print "c")  -- sequence of I/O *)
            SeqExp [
              box_node (ExpApp (print_fn, str_a));
              box_node (ExpApp (print_fn, str_b));
              box_node (ExpApp (print_fn, str_c))
            ]

            (* SML: (TextIO.output (file, "data"); TextIO.closeOut file; ())  *)
            SeqExp [
              write_data;
              close_file;
              box_node (TupleExp [])  (* return unit *)
            ]
          ]}

          {3 Semantics}

          - Expressions are evaluated strictly left-to-right
          - All intermediate results are discarded
          - Only the last expression's value is returned
          - Earlier expressions typically have type [unit] (but not required)
          - Result type is the type of the last expression

          {3 Type Checking}

          While not enforced, earlier expressions typically return [unit]:
          {[
            (* SML: (42; "hi")  -- legal but wasteful *)
            (* Type: string, but 42 is discarded *)

            (* Better style: *)
            (* (print "hi"; result)  -- side effect returns unit *)
          ]}

          The type checker may warn about non-unit expressions in non-final
          positions since their values are ignored.

          {3 Comparison with Let}

          Sequential expressions differ from let expressions:
          {[
            (* SeqExp: no bindings, just sequencing *)
            (* SML: (print "a"; print "b") *)
            SeqExp [print_a; print_b]

            (* LetExp: introduces bindings *)
            (* SML: let val x = f () in g x end *)
            LetExp ([val_dec_x], [g_x])
          ]}

          {3 Single Expression}

          A single expression in parentheses is NOT a sequence:
          {[
            (* SML: (42)  -- just parentheses *)
            ParenExp (box_node (ExpCon (box_node (ConInt (box_node "42")))))

            (* SML: (42;)  -- syntax error in SML *)

            (* SML: (42; 43)  -- THIS is a sequence *)
            SeqExp [int_42; int_43]
          ]}

          @see LetExp Local declarations
          @see ParenExp Single expression parentheses
          @see TupleExp Unit value [()] *)
  | LetExp of declaration node list * expression node list
      (** Local declaration - scoped bindings and definitions.

          Introduces local declarations (values, functions, types, etc.) that are
          visible only within the body expressions. This is SML's primary mechanism
          for introducing locally-scoped names and definitions.

          {3 SML Syntax}

          {v
            expression ::= let dec1 ... decn in exp1 ; ... ; expm end
                                               (* n >= 1, m >= 1 *)
          v}

          {3 Examples}

          {[
            (* SML: let val x = 1 in x + x end  -- simple value binding *)
            LetExp (
              [box_node (ValDec ([], box_node (ValBind (
                box_node (PatIdx (box_node (WithoutOp (box_node (IdxIdx (box_node "x")))))),
                box_node (ExpCon (box_node (ConInt (box_node "1")))),
                None
              ))))],
              [box_node (InfixApp (x_node, plus_op, x_node))]
            )

            (* SML: let val x = 10 val y = 20 in x * y end  -- multiple bindings *)
            LetExp (
              [
                box_node (ValDec ([], val_bind_x_10));
                box_node (ValDec ([], val_bind_y_20))
              ],
              [box_node (InfixApp (x_node, times_op, y_node))]
            )

            (* SML: let fun square x = x * x in square 5 end  -- function *)
            LetExp (
              [box_node (FunDec (box_node (FunBind (
                box_node (FunMatchPrefix (
                  box_node (WithoutOp (box_node (IdxIdx (box_node "square")))),
                  [x_pat],
                  None,
                  box_node (InfixApp (x_node, times_op, x_node)),
                  None
                )),
                None
              ))))],
              [box_node (ExpApp (square_fn, int_5))]
            )

            (* SML: let datatype tree = Leaf | Node of int * tree * tree
                       fun size Leaf = 0
                         | size (Node (_, l, r)) = 1 + size l + size r
                   in size myTree end  *)
            LetExp (
              [
                box_node (DatDec ([], datatype_tree));
                box_node (FunDec (fun_bind_size))
              ],
              [box_node (ExpApp (size_fn, my_tree))]
            )

            (* SML: let val _ = print "Computing..." in expensive () end *)
            LetExp (
              [box_node (ValDec ([], box_node (ValBind (
                box_node PatWildcard,
                print_computing,
                None
              ))))],
              [box_node (ExpApp (expensive_fn, unit_exp))]
            )
          ]}

          {3 Semantics}

          - Declarations are evaluated in order (left-to-right)
          - Each declaration can reference previous declarations in the same let
          - All declarations are in scope for the body expressions
          - Body expressions form an implicit sequence (if multiple)
          - Result is the value of the last body expression
          - Scope ends at [end] keyword

          {3 Scoping Rules}

          Later declarations can reference earlier ones:
          {[
            (* SML: let val x = 1 val y = x + 1 in y end *)
            (* 'y' can reference 'x' since it appears later *)

            (* SML: let val x = 1 val x = x + 1 in x end  -- shadowing *)
            (* Second 'x' shadows the first, evaluates to 2 *)
          ]}

          For mutually recursive definitions, use [and]:
          {[
            (* SML: let fun even 0 = true
                         | even n = odd (n - 1)
                       and odd 0 = false
                         | odd n = even (n - 1)
                   in even 10 end *)
          ]}

          {3 Multiple Body Expressions}

          Multiple body expressions form an implicit sequence:
          {[
            (* SML: let val x = ref 0 in x := 5; !x end *)
            LetExp (
              [box_node (ValDec ([], val_bind_x_ref))],
              [
                box_node (ExpApp (assign_x, int_5));
                box_node (ExpApp (deref_op, x_node))
              ]
            )
            (* Equivalent to: let val x = ref 0 in (x := 5; !x) end *)
          ]}

          {3 Type Declarations}

          Let expressions can include type and datatype declarations:
          {[
            (* SML: let type point = int * int
                       val origin = (0, 0) : point
                   in origin end *)
            LetExp (
              [
                box_node (TypDec ([], type_point_def));
                box_node (ValDec ([], val_bind_origin))
              ],
              [origin_exp]
            )
          ]}

          @see declaration Declaration types (ValDec, FunDec, DatDec, etc.)
          @see SeqExp Sequential expressions
          @see ExpApp Function application *)
  | TypedExp of expression node * typ node
      (** Type-annotated expression - explicit type constraint.

          Constrains the type of an expression by providing an explicit type
          annotation. This is useful for resolving type ambiguities (especially
          numeric overloading), documenting programmer intent, or forcing more
          specific types than the type inference would otherwise derive.

          {3 SML Syntax}

          {v
            expression ::= expression : typ
          v}

          {3 Examples}

          {[
            (* SML: ([] : int list)  -- disambiguate empty list type *)
            TypedExp (
              box_node (ListExp []),
              box_node (TypCon (
                [box_node (TypCon ([], box_node (IdxIdx (box_node "int"))))],
                box_node (IdxIdx (box_node "list"))
              ))
            )

            (* SML: (0 : real)  -- force 0 to be real instead of int *)
            TypedExp (
              box_node (ExpCon (box_node (ConInt (box_node "0")))),
              box_node (TypCon ([], box_node (IdxIdx (box_node "real"))))
            )

            (* SML: (fn x => x : int -> int)  -- annotate function type *)
            TypedExp (
              box_node (FnExp (box_node (Case (x_pat, x_exp, None)))),
              box_node (TypArr (int_typ, int_typ))
            )

            (* SML: (x + y : int)  -- constrain result of addition *)
            TypedExp (
              box_node (InfixApp (x, plus, y)),
              box_node (TypCon ([], box_node (IdxIdx (box_node "int"))))
            )

            (* SML: (NONE : string option)  -- specify option type *)
            TypedExp (
              box_node (ExpIdx (box_node (IdxIdx (box_node "NONE")))),
              box_node (TypCon (
                [box_node (TypCon ([], box_node (IdxIdx (box_node "string"))))],
                box_node (IdxIdx (box_node "option"))
              ))
            )
          ]}

          {3 Semantics}

          - Expression is type-checked against the provided type
          - Type annotation must be compatible with inferred type
          - If types don't match, compilation error occurs
          - No runtime overhead (annotations are erased after type checking)
          - Result value is the same as the annotated expression

          {3 Use Cases}

          {b Resolving Numeric Overloading}:
          {[
            (* SML: 1.0  vs  (1 : real) *)
            (* Without annotation, 1 defaults to int *)
            (* With annotation, 1 is real *)
          ]}

          {b Documenting Intent}:
          {[
            (* SML: fun process (data : int list) : string = ... *)
            (* Annotations serve as inline documentation *)
          ]}

          {b Forcing Monomorphism}:
          {[
            (* SML: val empty : int list = [] *)
            (* Without annotation: 'a list (polymorphic) *)
            (* With annotation: int list (monomorphic) *)
          ]}

          {b Disambiguating Record Types}:
          {[
            (* SML: {x = 1, y = 2} : point *)
            (* Multiple record types might have x and y fields *)
          ]}

          {3 Type Checking}

          The annotation must be a supertype or equal to the inferred type:
          {[
            (* SML: (42 : int)  -- OK, inferred type is int *)
            (* SML: (42 : real)  -- ERROR, int is not real *)
            (* SML: ([] : 'a list)  -- OK, 'a list generalizes int list *)
          ]}

          @see typ Type expressions
          @see LetExp Type declarations in let
          @see FnExp Function type annotations *)
  | RaiseExp of expression node
      (** Exception raising - abnormal control flow with exceptions.

          Raises an exception, immediately transferring control to the nearest
          enclosing exception handler (or terminating the program if unhandled).
          The expression must evaluate to a value of the built-in extensible
          [exn] type.

          {3 SML Syntax}

          {v
            expression ::= raise expression
          v}

          {3 Examples}

          {[
            (* SML: raise Fail "something went wrong"  -- standard exception *)
            RaiseExp (box_node (ExpApp (
              box_node (ExpIdx (box_node (IdxIdx (box_node "Fail")))),
              box_node (ExpCon (box_node (ConString (box_node "something went wrong"))))
            )))

            (* SML: raise Empty  -- nullary exception *)
            RaiseExp (box_node (ExpIdx (box_node (IdxIdx (box_node "Empty")))))

            (* SML: raise Subscript  -- standard Basis exception *)
            RaiseExp (box_node (ExpIdx (box_node (IdxIdx (box_node "Subscript")))))

            (* SML: if n < 0 then raise Domain else sqrt n *)
            IfExp (
              box_node (InfixApp (n, less_than, int_0)),
              box_node (RaiseExp (box_node (ExpIdx (domain_exn)))),
              box_node (ExpApp (sqrt_fn, n))
            )

            (* SML: raise MyException (x, y)  -- custom exception with data *)
            RaiseExp (box_node (ExpApp (
              my_exception_con,
              box_node (TupleExp [x_exp; y_exp])
            )))
          ]}

          {3 Semantics}

          - Expression is evaluated to produce an exception value
          - Control immediately transfers to nearest handler (see HandleExp)
          - If no handler matches, exception propagates up the call stack
          - If unhandled at top level, program terminates with error message
          - Type of raise expression is polymorphic: ['a]

          {3 Polymorphic Type}

          The [raise] expression can have any type, since it never returns
          normally:
          {[
            (* SML: (raise Fail "error") + 1  -- typechecks as int *)
            (* SML: if x < 0 then raise Domain else "ok"  -- typechecks as string *)
          ]}

          This allows [raise] to appear in any context.

          {3 Standard Exceptions}

          Common standard exceptions from the Basis Library:
          {[
            (* General exceptions *)
            raise Fail "message"     (* generic failure *)
            raise Domain             (* argument out of domain *)
            raise Size               (* size too large *)
            raise Overflow           (* arithmetic overflow *)
            raise Subscript          (* index out of bounds *)
            raise Chr                (* invalid character code *)

            (* Structure-specific exceptions *)
            raise Empty              (* empty data structure *)
            raise Option             (* NONE in valOf *)
            raise Match              (* pattern match failure *)
            raise Bind               (* binding failure *)
          ]}

          {3 Declaring Custom Exceptions}

          Exceptions are declared with [exception]:
          {[
            (* SML: exception NotFound *)
            (* Then: raise NotFound *)

            (* SML: exception InvalidInput of string *)
            (* Then: raise InvalidInput "bad value" *)
          ]}

          {3 Control Flow}

          Exception raising interrupts normal evaluation:
          {[
            (* SML: (print "before"; raise Fail "error"; print "after") *)
            (* Only "before" is printed; "after" is never reached *)

            SeqExp [
              print_before;
              box_node (RaiseExp fail_error);
              print_after  (* unreachable *)
            ]
          ]}

          @see HandleExp Exception handling
          @see ExnDec Exception declarations
          @see ExnSpec Exception specifications *)
  | HandleExp of expression node * matching node
      (** Exception handling - catching and recovering from exceptions.

          Evaluates an expression with an exception handler. If the expression
          raises an exception, it's pattern-matched against the handler clauses.
          The first matching pattern's expression is evaluated. If no pattern
          matches, the exception propagates to the next outer handler.

          {3 SML Syntax}

          {v
            expression ::= expression handle match
          v}

          {3 Examples}

          {[
            (* SML: f x handle Fail msg => (print msg; 0)  -- catch Fail *)
            HandleExp (
              box_node (ExpApp (f_fn, x_arg)),
              box_node (Case (
                box_node (PatApp (
                  box_node (WithoutOp (box_node (IdxIdx (box_node "Fail")))),
                  box_node (PatIdx (box_node (WithoutOp (box_node (IdxIdx (box_node "msg"))))))
                )),
                box_node (SeqExp [
                  box_node (ExpApp (print_fn, msg_exp));
                  box_node (ExpCon (box_node (ConInt (box_node "0"))))
                ]),
                None
              ))
            )

            (* SML: parse input handle Empty => [] | Fail _ => []  -- multiple handlers *)
            HandleExp (
              box_node (ExpApp (parse_fn, input)),
              box_node (Case (
                box_node (PatIdx (box_node (WithoutOp (empty_id)))),
                box_node (ListExp []),
                Some (box_node (Case (
                  box_node (PatApp (
                    box_node (WithoutOp (fail_id)),
                    box_node PatWildcard
                  )),
                  box_node (ListExp []),
                  None
                )))
              ))
            )

            (* SML: computation () handle exn => (cleanup (); raise exn)  -- cleanup then re-raise *)
            HandleExp (
              box_node (ExpApp (computation, unit)),
              box_node (Case (
                box_node (PatIdx (box_node (WithoutOp (exn_id)))),
                box_node (SeqExp [
                  box_node (ExpApp (cleanup, unit));
                  box_node (RaiseExp exn_exp)
                ]),
                None
              ))
            )

            (* SML: readFile path handle _ => defaultContent  -- catch all exceptions *)
            HandleExp (
              box_node (ExpApp (read_file, path)),
              box_node (Case (
                box_node PatWildcard,
                default_content,
                None
              ))
            )

            (* SML: hd xs handle Empty => 0 | Subscript => 1  *)
            HandleExp (
              box_node (ExpApp (hd_fn, xs)),
              box_node (Case (
                box_node (PatIdx (box_node (WithoutOp (empty_id)))),
                int_0,
                Some (box_node (Case (
                  box_node (PatIdx (box_node (WithoutOp (subscript_id)))),
                  int_1,
                  None
                )))
              ))
            )
          ]}

          {3 Semantics}

          - Expression is evaluated normally
          - If it completes without exception, that value is returned
          - If it raises an exception, match clauses are tried in order
          - First matching pattern's expression is evaluated and returned
          - If no pattern matches, exception propagates to outer handler
          - Type of handler clauses must match type of protected expression

          {3 Pattern Matching on Exceptions}

          Exception patterns work like regular patterns:
          {[
            (* Specific exception constructor *)
            handle Empty => ...

            (* Exception with data *)
            handle Fail msg => ...

            (* Multiple patterns *)
            handle Empty => ... | Overflow => ...

            (* Wildcard (catch all) *)
            handle _ => ...

            (* Variable binding (catch and name) *)
            handle exn => (logError exn; defaultValue)
          ]}

          {3 Scope and Nesting}

          Handlers can be nested, with inner handlers taking precedence:
          {[
            (* SML: (f x handle Empty => 0) handle Fail _ => 1 *)
            HandleExp (
              box_node (HandleExp (
                box_node (ExpApp (f, x)),
                box_node (Case (empty_pat, int_0, None))
              )),
              box_node (Case (fail_pat, int_1, None))
            )
            (* Empty caught by inner handler, Fail by outer *)
          ]}

          {3 Type Consistency}

          All handler branches must return the same type as the protected
          expression:
          {[
            (* SML: (1 + 2) handle Overflow => 0  -- OK, both int *)
            (* SML: (1 + 2) handle Overflow => "error"  -- ERROR, int vs string *)
          ]}

          {3 Comparison with Try-Catch}

          SML's handle is an expression (returns a value), unlike many languages
          where try-catch is a statement:
          {[
            (* SML: val result = f x handle Fail _ => defaultValue *)
            (* Handler is part of the expression, not separate syntax *)
          ]}

          @see RaiseExp Raising exceptions
          @see matching Match clause structure
          @see CaseExp Pattern matching *)
  | AndExp of expression node * expression node
      (** Short-circuit conjunction - lazy boolean AND.

          Evaluates [exp1], and only if it's [true], evaluates [exp2]. This is
          a short-circuiting operator: if [exp1] is [false], [exp2] is never
          evaluated. Semantically equivalent to [if exp1 then exp2 else false].

          {3 SML Syntax}

          {v
            expression ::= expression andalso expression
          v}

          {3 Examples}

          {[
            (* SML: x > 0 andalso y > 0  -- both conditions *)
            AndExp (
              box_node (InfixApp (x, greater_than, int_0)),
              box_node (InfixApp (y, greater_than, int_0))
            )

            (* SML: not (null xs) andalso hd xs > 0  -- guards against Empty *)
            AndExp (
              box_node (ExpApp (
                not_fn,
                box_node (ExpApp (null_fn, xs))
              )),
              box_node (InfixApp (
                box_node (ExpApp (hd_fn, xs)),
                greater_than,
                int_0
              ))
            )
            (* Second part only evaluated if xs is non-empty *)

            (* SML: valid andalso compute ()  -- conditional side effect *)
            AndExp (
              valid_exp,
              box_node (ExpApp (compute_fn, unit))
            )
            (* compute() called only if valid is true *)

            (* SML: x >= 0 andalso x <= 100  -- range check *)
            AndExp (
              box_node (InfixApp (x, greater_equal, int_0)),
              box_node (InfixApp (x, less_equal, int_100))
            )
          ]}

          {3 Semantics}

          - [exp1] is evaluated first
          - If [exp1] evaluates to [false], result is [false] (exp2 not evaluated)
          - If [exp1] evaluates to [true], [exp2] is evaluated
          - Result is the value of [exp2] (which must be boolean)
          - Both expressions must have type [bool]

          {3 Short-Circuit Behavior}

          The short-circuit property is crucial for avoiding errors:
          {[
            (* SML: not (null xs) andalso hd xs = 0 *)
            (* Safe: hd xs only evaluated when xs is non-empty *)

            (* SML: hd xs = 0 andalso not (null xs) *)
            (* UNSAFE: can raise Empty exception! *)
          ]}

          Order matters when one expression depends on the other's guard.

          {3 Desugaring}

          [andalso] is syntactic sugar for conditional:
          {v
            exp1 andalso exp2  =  if exp1 then exp2 else false
          v}

          The AST represents this as AndExp for convenience, but semantically
          they're equivalent.

          {3 Comparison with Eager Operators}

          Unlike some languages, SML has no eager boolean AND operator.
          [andalso] always short-circuits:
          {[
            (* SML: (always using short-circuit) *)
            false andalso (raise Fail "never")  (* returns false, no exception *)

            (* Some languages: & vs && distinction *)
            (* SML: only andalso exists, always short-circuits *)
          ]}

          {3 Associativity}

          [andalso] is right-associative (like [::] and [^]):
          {[
            (* SML: a andalso b andalso c *)
            (* Parses as: a andalso (b andalso c) *)
            AndExp (a, box_node (AndExp (b, c)))
          ]}

          @see OrExp Short-circuit disjunction
          @see IfExp Conditional expression
          @see ExpCon Boolean constants (true, false) *)
  | OrExp of expression node * expression node
      (** Short-circuit disjunction - lazy boolean OR.

          Evaluates [exp1], and only if it's [false], evaluates [exp2]. This is
          a short-circuiting operator: if [exp1] is [true], [exp2] is never
          evaluated. Semantically equivalent to [if exp1 then true else exp2].

          {3 SML Syntax}

          {v
            expression ::= expression orelse expression
          v}

          {3 Examples}

          {[
            (* SML: x < 0 orelse x > 100  -- out of range *)
            OrExp (
              box_node (InfixApp (x, less_than, int_0)),
              box_node (InfixApp (x, greater_than, int_100))
            )

            (* SML: null xs orelse hd xs = target  -- find element *)
            OrExp (
              box_node (ExpApp (null_fn, xs)),
              box_node (InfixApp (
                box_node (ExpApp (hd_fn, xs)),
                equals,
                target
              ))
            )
            (* Second part only evaluated if xs is non-empty *)

            (* SML: cached orelse expensive ()  -- memoization pattern *)
            OrExp (
              cached_exp,
              box_node (ExpApp (expensive_fn, unit))
            )
            (* expensive() called only if cached is false *)

            (* SML: isSome opt orelse default  -- fallback logic *)
            OrExp (
              box_node (ExpApp (is_some_fn, opt)),
              default_exp
            )
          ]}

          {3 Semantics}

          - [exp1] is evaluated first
          - If [exp1] evaluates to [true], result is [true] (exp2 not evaluated)
          - If [exp1] evaluates to [false], [exp2] is evaluated
          - Result is the value of [exp2] (which must be boolean)
          - Both expressions must have type [bool]

          {3 Short-Circuit Behavior}

          The short-circuit property enables safe chaining:
          {[
            (* SML: null xs orelse hd xs > 0 *)
            (* Safe: hd xs only evaluated when xs is non-empty *)

            (* SML: hd xs > 0 orelse null xs *)
            (* UNSAFE: can raise Empty exception! *)
          ]}

          {3 Desugaring}

          [orelse] is syntactic sugar for conditional:
          {v
            exp1 orelse exp2  =  if exp1 then true else exp2
          v}

          The AST represents this as OrExp for convenience.

          {3 Common Patterns}

          {b Validation with Fallback}:
          {[
            (* SML: validate input orelse useDefault () *)
            OrExp (
              box_node (ExpApp (validate, input)),
              box_node (ExpApp (use_default, unit))
            )
          ]}

          {b Multiple Conditions}:
          {[
            (* SML: isAdmin user orelse isOwner user orelse hasPermission user *)
            OrExp (
              is_admin,
              box_node (OrExp (is_owner, has_permission))
            )
          ]}

          {3 Associativity}

          [orelse] is right-associative:
          {[
            (* SML: a orelse b orelse c *)
            (* Parses as: a orelse (b orelse c) *)
            OrExp (a, box_node (OrExp (b, c)))
          ]}

          @see AndExp Short-circuit conjunction
          @see IfExp Conditional expression
          @see ExpCon Boolean constants (true, false) *)
  | IfExp of expression node * expression node * expression node
      (** Conditional expression - branch based on boolean condition.

          Evaluates the condition [exp1], then evaluates either [exp2] (if true)
          or [exp3] (if false), but never both. Both branches must have the same
          type, which becomes the type of the entire if expression.

          {3 SML Syntax}

          {v
            expression ::= if expression then expression else expression
          v}

          {3 Examples}

          {[
            (* SML: if n = 0 then 1 else n * fact(n-1)  -- factorial *)
            IfExp (
              box_node (InfixApp (n, equals, int_0)),
              box_node (ExpCon (box_node (ConInt (box_node "1")))),
              box_node (InfixApp (
                n,
                times,
                box_node (ExpApp (
                  fact_fn,
                  box_node (InfixApp (n, minus, int_1))
                ))
              ))
            )

            (* SML: if x > 0 then "positive" else "non-positive" *)
            IfExp (
              box_node (InfixApp (x, greater_than, int_0)),
              box_node (ExpCon (box_node (ConString (box_node "positive")))),
              box_node (ExpCon (box_node (ConString (box_node "non-positive"))))
            )

            (* SML: if null xs then [] else tl xs  -- safe tail *)
            IfExp (
              box_node (ExpApp (null_fn, xs)),
              box_node (ListExp []),
              box_node (ExpApp (tl_fn, xs))
            )

            (* SML: if debug then print ("x = " ^ Int.toString x) else ()  *)
            IfExp (
              debug_flag,
              box_node (ExpApp (print_fn, debug_msg)),
              box_node (TupleExp [])  (* unit value *)
            )

            (* SML: if a then if b then 1 else 2 else 3  -- nested *)
            IfExp (
              a_cond,
              box_node (IfExp (b_cond, int_1, int_2)),
              int_3
            )
          ]}

          {3 Semantics}

          - [exp1] (condition) is evaluated first
          - Must evaluate to a boolean value
          - If [true], [exp2] (then-branch) is evaluated and returned
          - If [false], [exp3] (else-branch) is evaluated and returned
          - Only one branch is ever evaluated (not both)
          - Type of [exp2] must equal type of [exp3]

          {3 Type Consistency}

          Both branches must have the same type:
          {[
            (* SML: if b then 1 else 2  -- OK, both int *)
            (* SML: if b then 1 else "no"  -- ERROR, int vs string *)
            (* SML: if b then () else ()  -- OK, both unit *)
          ]}

          {3 Unit Type in Conditionals}

          For side-effect-only branches, use unit [()] type:
          {[
            (* SML: if verbose then print msg else () *)
            IfExp (
              verbose,
              box_node (ExpApp (print_fn, msg)),
              box_node (TupleExp [])
            )
          ]}

          {3 Nested Conditionals}

          Multiple conditions can be chained:
          {[
            (* SML: if x < 0 then ~1 else if x = 0 then 0 else 1 *)
            IfExp (
              box_node (InfixApp (x, less, int_0)),
              int_neg_1,
              box_node (IfExp (
                box_node (InfixApp (x, equals, int_0)),
                int_0,
                int_1
              ))
            )
          ]}

          {3 Comparison with Case}

          Simple boolean tests use [if], complex pattern matching uses [case]:
          {[
            (* if: simple boolean condition *)
            (* SML: if x > 0 then "yes" else "no" *)

            (* case: pattern matching *)
            (* SML: case opt of NONE => 0 | SOME x => x *)
          ]}

          {3 Short-Circuit Alternatives}

          [andalso] and [orelse] can replace some conditionals:
          {[
            (* SML: if x then y else false  =  x andalso y *)
            (* SML: if x then true else y    =  x orelse y *)
          ]}

          @see CaseExp Pattern-based conditionals
          @see AndExp Short-circuit conjunction
          @see OrExp Short-circuit disjunction *)
  | WhileExp of expression node * expression node
      (** While loop - imperative iteration with condition.

          Repeatedly evaluates the body [exp2] as long as the condition [exp1]
          remains [true]. Always returns unit [()]. Useful only for side effects
          (mutation, I/O). SML's primary imperative looping construct.

          {3 SML Syntax}

          {v
            expression ::= while expression do expression
          v}

          {3 Examples}

          {[
            (* SML: while !r > 0 do r := !r - 1  -- countdown *)
            WhileExp (
              box_node (InfixApp (
                box_node (ExpApp (deref, r_ref)),
                greater_than,
                int_0
              )),
              box_node (ExpApp (
                assign_r,
                box_node (InfixApp (
                  box_node (ExpApp (deref, r_ref)),
                  minus,
                  int_1
                ))
              ))
            )

            (* SML: while not (TextIO.endOfStream input) do
                       print (TextIO.inputLine input)  *)
            WhileExp (
              box_node (ExpApp (
                not_fn,
                box_node (ExpApp (end_of_stream, input))
              )),
              box_node (ExpApp (
                print_fn,
                box_node (ExpApp (input_line, input))
              ))
            )

            (* SML: while true do ()  -- infinite loop *)
            WhileExp (
              box_node (ExpIdx (box_node (IdxIdx (box_node "true")))),
              box_node (TupleExp [])
            )

            (* SML: while !i < length arr do (
                       process (Array.sub (arr, !i));
                       i := !i + 1
                     )  *)
            WhileExp (
              box_node (InfixApp (
                box_node (ExpApp (deref, i_ref)),
                less_than,
                box_node (ExpApp (length_fn, arr))
              )),
              box_node (SeqExp [
                box_node (ExpApp (
                  process_fn,
                  box_node (ExpApp (array_sub, box_node (TupleExp [arr; deref_i])))
                ));
                box_node (ExpApp (
                  assign_i,
                  box_node (InfixApp (deref_i, plus, int_1))
                ))
              ])
            )
          ]}

          {3 Semantics}

          - [exp1] (condition) is evaluated
          - If [false], loop terminates and returns [()]
          - If [true], [exp2] (body) is evaluated (result discarded)
          - Process repeats with condition re-evaluation
          - Loop eventually returns unit when condition becomes [false]
          - Infinite loop if condition never becomes [false]

          {3 Type Requirements}

          - Condition [exp1] must have type [bool]
          - Body [exp2] typically has type [unit] (but not required)
          - Entire while expression has type [unit]

          {3 Side Effects}

          While loops are only useful with side effects:
          {[
            (* Mutation *)
            (* SML: while !n > 0 do n := !n - 1 *)

            (* I/O *)
            (* SML: while hasMore () do print (readNext ()) *)

            (* Exceptions *)
            (* SML: while true do if done () then raise Exit else work () *)
          ]}

          Without side effects, while loops compute nothing.

          {3 Termination}

          Ensure the condition eventually becomes false:
          {[
            (* GOOD: terminates when r reaches 0 *)
            (* while !r > 0 do r := !r - 1 *)

            (* BAD: infinite loop, r never changes *)
            (* while !r > 0 do print "looping" *)

            (* GOOD: explicit break via exception *)
            (* while true do if done () then raise Exit else work () *)
          ]}

          {3 Functional Alternatives}

          SML programmers prefer higher-order functions over loops:
          {[
            (* While loop style: *)
            (* let val i = ref 0
                   val sum = ref 0
               in while !i < n do (
                    sum := !sum + !i;
                    i := !i + 1
                  );
                  !sum
               end *)

            (* Functional style: *)
            (* List.foldl op+ 0 (List.tabulate (n, fn x => x)) *)
          ]}

          Functional code is shorter, clearer, and less error-prone.

          {3 Common Patterns}

          {b Input Processing}:
          {[
            (* Read until end of input *)
            while not (endOfInput ()) do
              processLine (readLine ())
          ]}

          {b Array Iteration}:
          {[
            (* Process all array elements *)
            let val i = ref 0
            in while !i < Array.length arr do (
                 f (Array.sub (arr, !i));
                 i := !i + 1
               )
            end
          ]}

          @see SeqExp Sequential evaluation
          @see IfExp Conditional branching
          @see ExpApp Reference operations (!r, r := v) *)
  | CaseExp of expression node * matching node
      (** Case analysis - pattern matching with multiple clauses.

          Evaluates the expression, then attempts to match it against a sequence
          of pattern clauses. The first matching pattern's expression is evaluated
          and returned. If no pattern matches, raises the [Match] exception.

          {3 SML Syntax}

          {v
            expression ::= case expression of match
            match      ::= pat => exp
                         | pat => exp | match
          v}

          {3 Examples}

          {[
            (* SML: case xs of [] => 0 | x::_ => x  -- head with default *)
            CaseExp (
              xs_node,
              box_node (Case (
                box_node (PatIdx (box_node (WithoutOp (nil_id)))),
                box_node (ExpCon (box_node (ConInt (box_node "0")))),
                Some (box_node (Case (
                  box_node (PatInfix (
                    box_node (PatIdx (box_node (WithoutOp (x_id)))),
                    cons_op,
                    box_node PatWildcard
                  )),
                  x_node,
                  None
                )))
              ))
            )

            (* SML: case opt of NONE => 0 | SOME n => n  -- option unwrap *)
            CaseExp (
              opt_node,
              box_node (Case (
                box_node (PatIdx (box_node (WithoutOp (none_id)))),
                int_0,
                Some (box_node (Case (
                  box_node (PatApp (
                    box_node (WithoutOp (some_id)),
                    box_node (PatIdx (box_node (WithoutOp (n_id))))
                  )),
                  n_node,
                  None
                )))
              ))
            )

            (* SML: case tree of
                     Leaf => 0
                   | Node (_, l, r) => 1 + size l + size r  *)
            CaseExp (
              tree_node,
              box_node (Case (
                box_node (PatIdx (box_node (WithoutOp (leaf_id)))),
                int_0,
                Some (box_node (Case (
                  box_node (PatApp (
                    box_node (WithoutOp (node_id)),
                    box_node (PatTuple [
                      box_node PatWildcard;
                      box_node (PatIdx (box_node (WithoutOp (l_id))));
                      box_node (PatIdx (box_node (WithoutOp (r_id))))
                    ])
                  )),
                  box_node (InfixApp (
                    int_1,
                    plus,
                    box_node (InfixApp (
                      box_node (ExpApp (size_fn, l_node)),
                      plus,
                      box_node (ExpApp (size_fn, r_node))
                    ))
                  )),
                  None
                )))
              ))
            )

            (* SML: case (x, y) of
                     (0, 0) => "origin"
                   | (0, _) => "y-axis"
                   | (_, 0) => "x-axis"
                   | _      => "other"  *)
            CaseExp (
              box_node (TupleExp [x_node; y_node]),
              box_node (Case (
                box_node (PatTuple [int_0_pat; int_0_pat]),
                box_node (ExpCon (box_node (ConString (box_node "origin")))),
                Some (box_node (Case (
                  box_node (PatTuple [int_0_pat; box_node PatWildcard]),
                  box_node (ExpCon (box_node (ConString (box_node "y-axis")))),
                  Some (box_node (Case (
                    box_node (PatTuple [box_node PatWildcard; int_0_pat]),
                    box_node (ExpCon (box_node (ConString (box_node "x-axis")))),
                    Some (box_node (Case (
                      box_node PatWildcard,
                      box_node (ExpCon (box_node (ConString (box_node "other")))),
                      None
                    )))
                  )))
                )))
              ))
            )
          ]}

          {3 Semantics}

          - Expression is evaluated to a value
          - Patterns are tried in order (top to bottom)
          - First pattern that matches determines which expression runs
          - Variables in the pattern are bound for the expression
          - If no pattern matches, raises [Match] exception
          - All branch expressions must have the same type

          {3 Pattern Matching}

          Case expressions support rich pattern matching:
          {[
            (* Constant patterns *)
            case n of 0 => ... | 1 => ... | _ => ...

            (* Constructor patterns *)
            case opt of NONE => ... | SOME x => ...

            (* Tuple patterns *)
            case pair of (x, y) => x + y

            (* Record patterns *)
            case point of {x, y} => x + y

            (* List patterns *)
            case xs of [] => ... | x::xs' => ...

            (* Nested patterns *)
            case tree of Node (Leaf, x, Leaf) => ...

            (* As patterns *)
            case xs of [] => ... | (x::_) as ys => ...
          ]}

          {3 Exhaustiveness}

          The compiler warns about non-exhaustive matches:
          {[
            (* Non-exhaustive: missing SOME case *)
            (* case opt of NONE => 0 *)
            (* Warning: match non-exhaustive, SOME not covered *)

            (* Exhaustive with wildcard *)
            (* case opt of NONE => 0 | _ => 1 *)
          ]}

          {3 Guards and Complex Conditions}

          SML doesn't have pattern guards. Use nested case or if:
          {[
            (* Want: case x of n if n > 0 => ...  (not valid SML) *)

            (* Instead: *)
            case x of n => if n > 0 then ... else ...

            (* Or: *)
            if x > 0 then
              case x of n => ...
            else ...
          ]}

          {3 Comparison with If}

          Use case for pattern matching, if for simple conditions:
          {[
            (* Good: case for data structure decomposition *)
            case xs of [] => ... | x::xs' => ...

            (* Good: if for boolean conditions *)
            if x > 0 then ... else ...

            (* Bad: case for simple boolean *)
            (* case x > 0 of true => ... | false => ... *)
          ]}

          {3 Type Consistency}

          All branches must have the same type:
          {[
            (* SML: case opt of NONE => 0 | SOME _ => 1  -- OK, both int *)
            (* SML: case opt of NONE => 0 | SOME _ => "yes"  -- ERROR *)
          ]}

          @see matching Match clause structure (Case constructor)
          @see pat Pattern syntax
          @see FnExp Anonymous functions (fn match)
          @see HandleExp Exception handling (similar structure) *)
  | FnExp of matching node
      (** Anonymous function: [fn match].

          Creates a function value from a match. Multi-argument functions use
          nested [fn] expressions or tuple patterns.

          {[
            (* SML: fn x => x + 1 *)
            FnExp (box_node (Case (x_pat, x_plus_1, None)))

            (* SML: fn [] => 0 | _::xs => 1 + length xs *)
            FnExp (box_node (Case (nil_pat, int_0,
              Some (box_node (Case (cons_pat, one_plus_length_xs, None))))))
          ]}

          @see 'matching' Match clause structure *)

(** {2 Expression Rows}

    {[
      exprow ::= lab = expression [, exprow]
    ]}

    Record rows bind labels to expressions in record literals.

    @see 'RecordExp' Record expression using rows *)
and row =
  | Row of idx node * expression node * row node option
      (** Expression row: [lab = expression].

          The optional third component allows chaining for parser convenience,
          though {!RecordExp} stores rows as a list.

          {[
            (* SML: x = 1 (in a record) *)
            Row
              ( box_node (IdxLab (box_node "x")),
                box_node (ExpCon (box_node (ConInt (box_node "1")))),
                None )
          ]} *)

(** {2 Match Clauses}

    {[
      match ::= pat => expression [| match]
    ]}

    Matches are used in {!CaseExp}, {!HandleExp}, and {!FnExp} expressions. Each
    clause binds pattern variables in the corresponding expression.

    {3 Example}
    {[
      (* SML: 0 => "zero" | _ => "other" *)
      Case
        ( box_node (PatCon (box_node (ConInt (box_node "0")))),
          box_node (ExpCon (box_node (ConString (box_node "zero")))),
          Some
            (box_node
               (Case
                  ( box_node PatWildcard,
                    box_node (ExpCon (box_node (ConString (box_node "other")))),
                    None ))) )
    ]}

    @see 'pat' Pattern syntax
    @see 'expression' Expression syntax *)
and matching =
  | Case of pat node * expression node * matching node option
      (** Match clause: [pat => expression].

          Components:
          - Pattern to match against
          - Expression to evaluate if pattern matches
          - Optional next clause ([| pat2 => exp2])

          Pattern variables bound in [pat] are visible in [expression]. *)

(** {2 Declarations}

    {[
      declaration ::= val [var,] valbind                                  (* value *)
            | fun [var,] funbind                                  (* function *)
            | type typbind                                        (* type *)
            | datatype datbind [withtype typbind]                 (* data type *)
            | datatype id = datatype longid                       (* data type replication *)
            | abstype datbind [withtype typbind] with declaration end     (* abstract type *)
            | exception exnbind                                   (* exception *)
            | structure strbind                                   (* structure, not in expressions *)
            |                                                     (* empty *)
            | dec1 [;] dec2                                       (* sequence *)
            | local dec1 in dec2 end                              (* local *)
            | open longid1 ... longidn                            (* inclusion, n >= 1 *)
            | nonfix id1 ... idn                                  (* nonfix, n >= 1 *)
            | infix [digit] id1 ... idn                           (* left-associative infix, n >= 1 *)
            | infixr [digit] id1 ... idn                          (* right-associative infix, n >= 1 *)
    ]}

    Declarations introduce new bindings into scope. They appear at the top
    level, in [let] expressions, and in structures.

    @see 'value_binding' Value binding details
    @see 'function_binding' Function binding details
    @see 'type_binding' Type binding details
    @see 'data_binding' Datatype binding details *)
and declaration =
  | ValDec of idx node list * value_binding node
      (** Value declaration: [val [var,] valbind].

          The [idx list] contains explicit type variables for polymorphism.
          These scope over all bindings in the [valbind].

          {[
            (* SML: val x = 42 *)
            ValDec ([], box_node (ValBind (x_pat, int_42, None)))

            (* SML: val 'a id : 'a -> 'a = fn x => x *)
            ValDec ([alpha], box_node (ValBind (id_pat, fn_x_x, None)))
          ]} *)
  | FunDec of function_binding node
      (** Function declaration: [fun [var,] funbind].

          Syntactic sugar for recursive function definitions. Multiple clauses
          enable pattern matching on arguments.

          {[
            (* SML: fun fact 0 = 1 | fact n = n * fact(n-1) *)
            FunDec (box_node (FunBind (
              box_node (FunMatchPrefix (...)),
              None
            )))
          ]}

          @see 'function_binding' Function binding structure
          @see 'fun_match' Individual clauses *)
  | TypDec of type_binding node
      (** Type abbreviation: [type typbind].

          Introduces type synonyms without creating new types.

          {[
            (* SML: type 'a pair = 'a * 'a *)
            TypDec (box_node (TypBind ([ alpha ], pair_id, pair_typ, None)))
          ]}

          @see 'type_binding' Type binding structure *)
  | DatDec of data_binding node * type_binding node option
      (** Datatype declaration: [datatype datbind [withtype typbind]].

          Creates new algebraic data types with constructors. The optional
          [withtype] allows mutually recursive type abbreviations.

          {[
            (* SML: datatype 'a tree = Leaf | Node of 'a tree * 'a * 'a tree *)
            DatDec
              ( box_node
                  (DatBind
                     ( [ alpha ],
                       tree_id,
                       box_node
                         (ConBind
                            ( leaf_id,
                              None,
                              Some
                                (box_node
                                   (ConBind (node_id, Some node_typ, None))) )),
                       None )),
                None )
          ]}

          @see 'data_binding' Datatype binding structure
          @see 'constructor_binding' Constructor binding structure *)
  | DataDecAlias of idx node * idx node
      (** Datatype replication: [datatype id = datatype longid].

          Makes [id] an alias for an existing datatype, including all its
          constructors.

          {[
            (* SML: datatype mybool = datatype bool *)
            DataDecAlias
              ( box_node (IdxIdx (box_node "mybool")),
                box_node (IdxIdx (box_node "bool")) )
          ]} *)
  | AbstractDec of
      data_binding node * type_binding node option * declaration node list
      (** Abstract type:
          [abstype datbind [withtype typbind] with declaration end].

          The datatype constructors are hidden outside the [with] block. Only
          the type name and functions defined in [declaration] are visible.

          {[
            (* SML: abstype 'a set = Set of 'a list with val empty = Set [] end *)
            AbstractDec (set_datbind, None, [ val_empty_dec ])
          ]} *)
  | ExnDec of exn_bind node
      (** Exception declaration: [exception exnbind].

          Creates new exception constructors.

          {[
            (* SML: exception NotFound *)
            ExnDec (box_node (ExnBind (not_found_id, None, None)))

            (* SML: exception Error of string *)
            ExnDec (box_node (ExnBind (error_id, Some string_typ, None)))
          ]}

          @see 'exn_bind' Exception binding structure *)
  | StrDec of structure_binding node
      (** Structure declaration: [structure strbind].

          Not allowed inside [let] expressions (only at top level).

          {[
            (* SML: structure S = struct val x = 1 end *)
            StrDec (box_node (StrBind (s_id, None, None)))
          ]}

          @see 'structure_binding' Structure binding structure
          @see 'structure' Structure expressions *)
  | SeqDec of declaration node list
      (** Sequence of declarations: [dec1 ; dec2 ; ...].

          Declarations are processed left-to-right, with each declaration's
          bindings visible in subsequent ones.

          {[
            (* SML: val x = 1; val y = x + 1 *)
            SeqDec [ val_x_dec; val_y_dec ]
          ]} *)
  | LocalDec of declaration node * declaration node
      (** Local declaration: [local dec1 in dec2 end].

          [dec1] is visible only within [dec2]. The bindings from [dec2] are
          exported; [dec1] bindings are hidden.

          {[
            (* SML: local val pi = 3.14 in fun area r = pi * r * r end *)
            LocalDec (val_pi_dec, fun_area_dec)
          ]} *)
  | OpenDec of idx node list
      (** Structure inclusion: [open longid1 ... longidn].

          Makes all bindings from the structures directly visible without
          qualification.

          {[
            (* SML: open List Option *)
            OpenDec [ list_id; option_id ]
          ]} *)
  | ExpDec of expression node
      (** Top-level expression: any expression at program top level.

          SML allows arbitrary expressions at the top level, which are evaluated
          for their side effects (e.g., ref assignments, I/O).

          {[
            (* SML: Control.Print.printDepth := 50; *)
            ExpDec (box_node (ExpApp (
              box_node (ExpIdx (box_node (IdxLong [...]))),
              box_node (ExpCon (box_node (ConInt (box_node "50"))))
            )))
          ]} *)
  | FixityDec of fixity node * idx node list
      (** Fixity declaration for operators.

          Specifies infix status and precedence for identifiers.

          {[
            (* SML: infix 6 + - *)
            FixityDec (box_node (Infix (box_node 6)), [plus_id; minus_id])

            (* SML: infixr 5 :: @ *)
            FixityDec (box_node (Infixr (box_node 5)), [cons_id; append_id])
          ]}

          @see 'fixity' Fixity specifications *)

(** Operator fixity specifications.

    {[
      nonfix id1 ... idn                  (* remove infix status *)
      infix [digit] id1 ... idn           (* left-associative, precedence 0-9 *)
      infixr [digit] id1 ... idn          (* right-associative, precedence 0-9 *)
    ]}

    Precedence defaults to 0 if not specified. Higher numbers bind tighter.

    Standard SML precedences:
    - 7: [*], [/], [div], [mod]
    - 6: [+], [-], [^]
    - 5: [::], [@]
    - 4: [=], [<>], [<], [>], [<=], [>=]
    - 3: [:=], [o]
    - 0: [before] *)
and fixity =
  | Nonfix
      (** Remove infix status: [nonfix id].

          The identifier becomes a regular prefix identifier. *)
  | Infix of int node
      (** Left-associative infix: [infix [n] id].

          Precedence 0-9, default 0. Left-associative means [a op b op c] parses
          as [(a op b) op c]. *)
  | Infixr of int node
      (** Right-associative infix: [infixr [n] id].

          Precedence 0-9, default 0. Right-associative means [a op b op c]
          parses as [a op (b op c)]. *)

(** Value bindings.

    {[
      valbind ::= pat = expression [and valbind]     (* destructuring *)
                | rec valbind                 (* recursive *)
    ]}

    @see 'ValDec' Value declarations using these bindings *)
and value_binding =
  | ValBind of pat node * expression node * value_binding node option
      (** Destructuring binding: [pat = expression].

          Pattern variables are bound to corresponding parts of the value.
          Multiple bindings with [and] are simultaneous (not sequential).

          {[
            (* SML: val (x, y) = (1, 2) *)
            ValBind (tuple_pat_xy, tuple_exp_12, None)

            (* SML: val x = 1 and y = 2 *)
            ValBind (x_pat, int_1, Some (box_node (ValBind (y_pat, int_2, None))))
          ]} *)
  | ValBindRec of value_binding node
      (** Recursive binding: [rec valbind].

          Allows the bound names to be used in the expressions. Required for
          recursive functions defined with [val].

          {[
            (* SML: val rec fact = fn n => if n = 0 then 1 else n * fact(n-1) *)
            ValBindRec (box_node (ValBind (fact_pat, fact_fn, None)))
          ]} *)

(** Function bindings.

    {[
      funbind ::= funmatch [and funbind]      (* clausal function *)
    ]}

    @see 'FunDec' Function declarations
    @see 'fun_match' Individual function clauses *)
and function_binding =
  | FunBind of fun_match node * function_binding node option
      (** Function binding with clauses.

          Additional bindings ([and ...]) define mutually recursive functions.

          {[
            (* SML: fun even 0 = true | even n = odd(n-1)
                    and odd 0 = false | odd n = even(n-1) *)
            FunBind (even_match, Some (box_node (FunBind (odd_match, None))))
          ]} *)

(** Function match clauses.

    {[
      funmatch ::= [op] id pat1 ... patn [: typ] = expression [| funmatch]    (* nonfix, n >= 1 *)
                 | pat1 id pat2 [: typ] = expression [| funmatch]             (* infix *)
                 | ( pat1 id pat2 ) pat'1 ... pat'n [: typ] = expression [| funmatch]  (* infix, n >= 0 *)
    ]}

    Multiple clauses define pattern matching on function arguments. All clauses
    must use the same function name and arity.

    @see 'pat' Pattern syntax for function parameters *)
and fun_match =
  | FunMatchPrefix of
      with_op node
      * pat node list
      * typ node option
      * expression node
      * fun_match node option
      (** Prefix (nonfix) function clause:
          [[op] id pat1 ... patn [: typ] = expression].

          The {!with_op} contains the function name (with optional [op] prefix).

          {[
            (* SML: fun length [] = 0 | length (_::xs) = 1 + length xs *)
            FunMatchPrefix
              ( box_node (WithoutOp length_id),
                [ nil_pat ],
                None,
                int_0,
                Some
                  (box_node
                     (FunMatchPrefix
                        ( box_node (WithoutOp length_id),
                          [ cons_pat ],
                          None,
                          one_plus_length_xs,
                          None ))) )
          ]} *)
  | FunMatchInfix of
      pat node
      * idx node
      * pat node
      * typ node option
      * expression node
      * fun_match node option
      (** Infix function clause: [pat1 id pat2 [: typ] = expression].

          The identifier [id] must be declared infix.

          {[
            (* SML: fun x ++ y = x @ y *)
            FunMatchInfix (x_pat, plusplus_id, y_pat, None, append_exp, None)
          ]} *)
  | FunMatchLow of
      pat node
      * idx node
      * pat node
      * pat node list
      * typ node option
      * expression node
      * fun_match node option
      (** Curried infix clause:
          [( pat1 id pat2 ) pat'1 ... pat'n [: typ] = expression].

          An infix operator with additional curried arguments.

          {[
            (* SML: fun (x ++ y) z = x @ y @ z *)
            FunMatchLow
              (x_pat, plusplus_id, y_pat, [ z_pat ], None, concat_exp, None)
          ]} *)

(** Type bindings.

    {[
      typbind ::= [var,] id = typ [and typbind]
    ]}

    Introduces type abbreviations (synonyms).

    @see 'TypDec' Type declarations *)
and type_binding =
  | TypBind of idx node list * idx node * typ node * type_binding node option
      (** Type abbreviation: [[var,] id = typ].

          Components:
          - Type parameters (e.g., [['a]] for ['a list])
          - Type name
          - Type definition
          - Optional additional bindings ([and ...])

          {[
            (* SML: type intpair = int * int *)
            TypBind ([], intpair_id, int_times_int, None)

            (* SML: type 'a pair = 'a * 'a *)
            TypBind ([alpha], pair_id, alpha_times_alpha, None)
          ]} *)

(** Datatype bindings.

    {[
      datbind ::= [var,] id = conbind [and datbind]
    ]}

    Introduces new algebraic data types with constructors.

    @see 'DatDec' Datatype declarations
    @see 'constructor_binding' Constructor bindings *)
and data_binding =
  | DatBind of
      idx node list
      * idx node
      * constructor_binding node
      * data_binding node option
      (** Datatype binding: [[var,] id = conbind].

          Components:
          - Type parameters
          - Type name
          - Constructor bindings
          - Optional additional bindings ([and ...] for mutual recursion)

          {[
            (* SML: datatype 'a option = NONE | SOME of 'a *)
            DatBind
              ( [ alpha ],
                option_id,
                box_node
                  (ConBind
                     ( none_id,
                       None,
                       Some (box_node (ConBind (some_id, Some alpha_typ, None)))
                     )),
                None )
          ]} *)

(** Constructor bindings.

    {[
      conbind ::= id [of typ] [| conbind]
    ]}

    Defines data constructors for a datatype.

    @see 'data_binding' Datatype bindings using constructors *)
and constructor_binding =
  | ConBind of idx node * typ node option * constructor_binding node option
      (** Constructor: [id [of typ]].

          Components:
          - Constructor name (conventionally capitalized)
          - Optional argument type
          - Optional next constructor ([| ...])

          {[
            (* SML: Nil (nullary constructor) *)
            ConBind (nil_id, None, None)

            (* SML: Cons of 'a * 'a list *)
            ConBind (cons_id, Some (TypTuple [alpha; alpha_list]), None)
          ]} *)

(** Exception bindings.

    {[
      exnbind ::= id [of typ] [and exnbind]     (* generative *)
                | id = longid [and exnbind]     (* renaming *)
    ]}

    @see 'ExnDec' Exception declarations *)
and exn_bind =
  | ExnBind of idx node * typ node option * exn_bind node option
      (** Generative exception: [id [of typ]].

          Creates a new, unique exception constructor.

          {[
            (* SML: exception Empty *)
            ExnBind (empty_id, None, None)

            (* SML: exception ParseError of string * int *)
            ExnBind (parse_error_id, Some string_int_typ, None)
          ]} *)
  | ExnBindAlias of idx node * idx node * exn_bind node option
      (** Exception renaming: [id = longid].

          Makes [id] an alias for an existing exception. Both names refer to the
          same exception identity.

          {[
            (* SML: exception E = SomeModule.SomeException *)
            ExnBindAlias (e_id, some_module_exn_id, None)
          ]} *)

(** {1 Module Language}

    The module language provides structures (collections of declarations),
    signatures (types of structures), and functors (parameterized structures).

    The module system enables:
    - Namespace management via structures
    - Interface specification via signatures
    - Generic programming via functors
    - Information hiding via opaque signatures

    @see 'structure' Structure expressions
    @see 'signature' Signature expressions
    @see 'functor_binding' Functor definitions *)

(** {2 Structures}

    {[
      structure ::= longid                        (* identifier *)
            | struct declaration end                (* structure *)
            | structure : sig                     (* transparent annotation *)
            | structure :> sig                    (* opaque annotation *)
            | id ( structure )                    (* functor application *)
            | id ( declaration )                    (* functor application *)
            | let declaration in structure end            (* local declaration *)
    ]}

    Structures are the values of the module language. They contain types,
    values, exceptions, and nested structures.

    @see 'declaration' Declarations within structures
    @see 'anotate' Transparent vs opaque sealing *)
and structure =
  | StrIdx of idx node
      (** Structure identifier (possibly qualified).

          {[
            (* SML: List (reference to existing structure) *)
            StrIdx (box_node (IdxIdx (box_node "List")))

            (* SML: Outer.Inner *)
            StrIdx (box_node (IdxLong [outer_id; inner_id]))
          ]} *)
  | StructStr of declaration node
      (** Structure expression: [struct declaration end].

          {[
            (* SML: struct val x = 1 type t = int end *)
            StructStr (box_node (SeqDec [ val_x; type_t ]))
          ]} *)
  | AnotateStr of idx node * anotate node * structure node
      (** Annotated structure: [structure : sig] or [structure :> sig].

          Note: The first component is the structure name for binding context.

          {[
            (* SML: S : SIG *)
            AnotateStr (s_id, box_node Transparent, sig_node)

            (* SML: S :> SIG (opaque) *)
            AnotateStr (s_id, box_node Opaque, sig_node)
          ]}

          @see 'anotate' Transparent vs opaque semantics *)
  | FunctorApp of idx node * structure node
      (** Functor application: [id ( structure )].

          {[
            (* SML: MkSet(IntOrd) *)
            FunctorApp (mkset_id, box_node (StrIdx intord_id))
          ]} *)
  | FunctorAppAnonymous of idx node * declaration node
      (** Functor application with anonymous argument: [id ( declaration )].

          The declarations form an anonymous structure.

          {[
            (* SML: MkSet(type t = int val compare = Int.compare) *)
            FunctorAppAnonymous
              (mkset_id, box_node (SeqDec [ type_t; val_compare ]))
          ]} *)
  | LocalDec of declaration node * structure node
      (** Local declaration in structure: [let declaration in structure end].

          {[
            (* SML: let val helper = ... in struct ... end end *)
            LocalDec (helper_dec, struct_body)
          ]} *)

(** Signature annotations for structures.

    Transparent annotation ([: sig]) preserves type equalities. Opaque
    annotation ([:> sig]) hides type implementations.

    {3 Example}
    {[
      (* Transparent: outside code knows S.t = int *)
      structure S : sig type t val x : t end = struct type t = int val x = 42 end

      (* Opaque: outside code only knows S.t exists *)
      structure S :> sig type t val x : t end = struct type t = int val x = 42 end
    ]} *)
and anotate =
  | Transparent
      (** Transparent annotation: [structure : sig].

          Type equalities are visible outside the structure. If [type t = int]
          inside, then [t = int] is known outside. *)
  | Opaque
      (** Opaque annotation: [structure :> sig].

          Abstract types hide their implementations. If [type t = int] inside,
          outside only knows [type t] exists. *)

(** Structure bindings.

    {[
      strbind ::= id [:[:>] sig] = structure [and strbind]
    ]}

    Structure bindings associate identifiers with structure values, optionally
    constraining them with signatures. Multiple bindings can be mutually
    recursive using [and].

    {3 Signature Constraints}
    - No constraint: All types and values are visible
    - Transparent ([:sig]): Type definitions visible, signature enforced
    - Opaque ([:>sig]): Type definitions hidden, abstract types created

    @see 'StrDec' Structure declarations
    @see 'anotate' Transparent vs opaque sealing *)
and structure_binding =
  | StrBind of
      idx node
      * (anotate node * signature node) option
      * structure node
      * structure_binding node option
      (** Structure binding: [id [:[\:>] sig] = structure].

          Binds a structure name to a structure expression, optionally
          constraining it with a signature for information hiding.

          Components:
          - Structure name (conventionally capitalized)
          - Optional signature constraint:
          - [None]: No constraint, all definitions visible
          - [Some (Transparent, sig)]: [:sig] - type equalities preserved
          - [Some (Opaque, sig)]: [:>sig] - abstract types created
          - Structure expression (the implementation)
          - Optional additional bindings ([and ...] for mutual recursion)

          {3 Examples}

          {[
            (* SML: structure S = struct val x = 1 end *)
            StrBind (
              box_node (IdxIdx (box_node "S")),
              None,
              box_node (StructStr (box_node (ValDec (...)))),
              None
            )

            (* SML: structure S : SIG = struct ... end (transparent) *)
            StrBind (
              s_id,
              Some (box_node Transparent, sig_node),
              struct_body,
              None
            )

            (* SML: structure S :> SIG = struct ... end (opaque) *)
            StrBind (
              s_id,
              Some (box_node Opaque, sig_node),
              struct_body,
              None
            )

            (* SML: structure A = ... and B = ... (mutually recursive) *)
            StrBind (a_id, None, a_body,
              Some (box_node (StrBind (b_id, None, b_body, None))))
          ]}

          @see 'anotate' Transparent vs opaque sealing semantics
          @see 'structure' Structure expression forms *)

(** {2 Signatures}

    {[
      sig ::= id                                   (* identifier *)
            | sig specification end                         (* signature *)
            | sig where type typrefin              (* refinement *)
    ]}

    Signatures are the types of structures. They specify what types and values a
    structure must provide.

    @see 'specification' Specifications within signatures
    @see 'typ_refine' Type refinements *)
and signature =
  | SignIdx of idx node
      (** Signature identifier.

          {[
            (* SML: ORDERED (reference to signature) *)
            SignIdx (box_node (IdxIdx (box_node "ORDERED")))
          ]} *)
  | SignSig of specification node list
      (** Signature expression: [sig specification end].

          Contains a list of specifications that structures matching this
          signature must fulfill.

          {[
            (* SML: sig val x : int type t end *)
            SignSig [ val_x_spec; type_t_spec ]
          ]} *)
  | SignWhere of signature node * typ_refine node
      (** Signature with type refinement: [sig where type typrefin].

          {[
            (* SML: ORD where type t = int *)
            SignWhere
              ( box_node (SignIdx ord_id),
                box_node (TypRef ([], t_id, int_typ, None)) )
          ]}

          @see 'typ_refine' Type refinement syntax *)

(** Type refinements in [where type] clauses.

    {[
      typrefin ::= [var,] longid = typ [and type typrefin]
    ]}

    Refines abstract types in a signature to specific types. *)
and typ_refine =
  | TypRef of
      idx node list * idx node * typ node * (typ node * typ_refine node) option
      (** Type refinement: [[var,] longid = typ].

          Components:
          - Type parameters (for parameterized type refinement)
          - Type path being refined
          - Concrete type definition
          - Optional additional refinements ([and type ...])

          {[
            (* SML: where type t = int *)
            TypRef ([], t_id, int_typ, None)

            (* SML: where type 'a container = 'a list *)
            TypRef ([alpha], container_id, alpha_list_typ, None)
          ]} *)

(** {2 Specifications}

    {[
      specification ::= val valdesc                                          (* value *)
             | type typdesc                                         (* type *)
             | eqtype typdesc                                       (* equality type *)
             | type typbind                                         (* type abbreviation *)
             | datatype datdesc                                     (* data type *)
             | datatype id = datatype longid                        (* data type replication *)
             | exception exndesc                                    (* exception *)
             | structure strdesc                                    (* structure *)
             |                                                      (* empty *)
             | spec1 [;] spec2                                      (* sequence *)
             | include sig                                          (* inclusion *)
             | include id1 ... idn                                  (* inclusion, n >= 1 *)
             | specification sharing type longid1 = ... = longidn            (* type sharing, n >= 2 *)
             | specification sharing longid1 = ... = longidn                 (* structure sharing, n >= 2 *)
    ]}

    Specifications describe the required contents of a structure. They appear
    within signature expressions.

    @see 'signature' Signatures containing specifications
    @see 'val_specification' Value descriptions
    @see 'typ_specification' Type descriptions *)
and specification =
  | SpecVal of val_specification node
      (** Value specification: [val valdesc].

          {[
            (* SML: val length : 'a list -> int *)
            SpecVal (box_node (ValDesc (length_id, list_to_int_typ, None)))
          ]} *)
  | SpecTyp of typ_specification node
      (** Abstract type specification: [type typdesc].

          Declares a type without revealing its implementation.

          {[
            (* SML: type t *)
            SpecTyp (box_node (TypDesc ([], t_id, None)))

            (* SML: type 'a container *)
            SpecTyp (box_node (TypDesc ([alpha], container_id, None)))
          ]} *)
  | SpecEqtyp of typ_specification node
      (** Equality type specification: [eqtype typdesc].

          Specifies types that admit equality ([=] and [<>]).

          {[
            (* SML: eqtype key *)
            SpecEqtyp (box_node (TypDesc ([], key_id, None)))
          ]} *)
  | SpecTypBind of type_binding node
      (** Type abbreviation in signature: [type typbind].

          Reveals the type definition, unlike abstract [type].

          {[
            (* SML: type pair = int * int *)
            SpecTypBind (box_node (TypBind ([], pair_id, int_times_int, None)))
          ]} *)
  | SpecDat of dat_specification node
      (** Datatype specification: [datatype datdesc].

          {[
            (* SML: datatype 'a option = NONE | SOME of 'a *)
            SpecDat
              (box_node (DatDesc ([ alpha ], option_id, constructors, None)))
          ]}

          @see 'dat_specification' Datatype description structure *)
  | SpecDatAlias of idx node * idx node
      (** Datatype replication: [datatype id = datatype longid]. *)
  | SpecExn of exn_specification node
      (** Exception specification: [exception exndesc].

          {[
            (* SML: exception NotFound *)
            SpecExn (box_node (ExnDesc (not_found_id, None, None)))
          ]}

          @see 'exn_specification' Exception description structure *)
  | SpecStr of str_specification node
      (** Structure specification: [structure strdesc].

          {[
            (* SML: structure Sub : SIG *)
            SpecStr (box_node (StrDesc (sub_id, sig_node, None)))
          ]}

          @see 'str_specification' Structure description structure *)
  | SpecSeq of specification node * specification node
      (** Sequence of specifications: [spec1 ; spec2]. *)
  | SpecInclude of signature node
      (** Include signature: [include sig].

          Incorporates all specifications from another signature.

          {[
            (* SML: include ORDERED *)
            SpecInclude (box_node (SignIdx ordered_id))
          ]} *)
  | SpecIncludeIdx of idx node list
      (** Include multiple signatures: [include id1 ... idn].

          {[
            (* SML: include ORD EQ *)
            SpecIncludeIdx [ ord_id; eq_id ]
          ]} *)
  | SpecSharingTyp of specification node * idx node list
      (** Type sharing constraint:
          [specification sharing type longid1 = ... = longidn].

          Asserts that the named types are the same type.

          {[
            (* SML: ... sharing type A.t = B.t *)
            SpecSharingTyp (base_spec, [ a_t_id; b_t_id ])
          ]} *)
  | SpecSharingStr of specification node * idx node list
      (** Structure sharing constraint:
          [specification sharing longid1 = ... = longidn].

          Asserts that the named structures share all type components. *)

(** Value descriptions in signatures.

    {[
      valdesc ::= id : typ [and valdesc]
    ]} *)
and val_specification =
  | ValDesc of idx node * typ node * val_specification node option
      (** Value description: [id : typ].

          {[
            (* SML: val f : int -> int *)
            ValDesc (f_id, int_to_int_typ, None)
          ]} *)

(** Type descriptions (abstract types) in signatures.

    {[
      typdesc ::= [var,] id [and typdesc]
    ]} *)
and typ_specification =
  | TypDesc of idx node list * idx node * typ_specification node option
      (** Type description: [[var,] id].

          Declares an abstract type with given arity (number of parameters).

          {[
            (* SML: type t (nullary) *)
            TypDesc ([], t_id, None)

            (* SML: type ('a, 'b) map (binary) *)
            TypDesc ([alpha; beta], map_id, None)
          ]} *)

(** Datatype descriptions in signatures.

    {[
      datdesc ::= [var,] id = condesc [and datdesc]
    ]} *)
and dat_specification =
  | DatDesc of
      idx node list
      * idx node
      * con_specification node
      * dat_specification node option
      (** Datatype description with constructors.

          Like {!data_binding} but in signature context.

          @see 'con_specification' Constructor descriptions *)

(** Constructor descriptions in signatures.

    {[
      condesc ::= id [of typ] [| condesc]
    ]} *)
and con_specification =
  | ConDesc of idx node * typ node option * con_specification node option
      (** Constructor description: [id [of typ]].

          Like {!constructor_binding} but in signature context. *)

(** Exception descriptions in signatures.

    {[
      exndesc ::= id [of typ] [and exndesc]
    ]} *)
and exn_specification =
  | ExnDesc of idx node * typ node option * exn_specification node option
      (** Exception description: [id [of typ]].

          Like {!exn_bind} but in signature context. *)

(** Structure descriptions in signatures.

    {[
      strdesc ::= id : sig [and strdesc]
    ]} *)
and str_specification =
  | StrDesc of idx node * signature node * str_specification node option
      (** Structure description: [id : sig].

          {[
            (* SML: structure Sub : SIG *)
            StrDesc (sub_id, sig_node, None)
          ]} *)

(** {2 Types}

    {[
      typ ::= var                          (* variable *)
            | [typ,] longid                (* constructor *)
            | ( typ )                      (* parentheses *)
            | typ1 -> typ2                 (* function *)
            | typ1 * ... * typn            (* tuple, n >= 2 *)
            | { [typrow] }                 (* record *)
    ]}

    Type expressions describe the types of values. They appear in type
    annotations, type definitions, and signatures.

    @see 'idx' Type variables (IdxVar)
    @see 'typ_row' Record type rows *)
and typ =
  | TypVar of idx node
      (** Type variable: ['var] or [''var].

          {[
            (* SML: 'a *)
            TypVar (box_node (IdxVar (box_node "a")))

            (* SML: ''a (equality type variable) *)
            TypVar (box_node (IdxVar (box_node "'a")))
          ]} *)
  | TypCon of typ node list * idx node
      (** Type constructor application: [[typ,] longid].

          {[
            (* SML: int *)
            TypCon ([], int_id)

            (* SML: int list *)
            TypCon ([int_typ], list_id)

            (* SML: (int, string) either *)
            TypCon ([int_typ; string_typ], either_id)
          ]} *)
  | TypPar of typ node
      (** Parenthesized type: [( typ )].

          Preserved for pretty-printing fidelity. *)
  | TypFun of typ node * typ node
      (** Function type: [typ1 -> typ2]. Right-associative.

          {[
            (* SML: int -> int *)
            TypFun (int_typ, int_typ)

            (* SML: int -> int -> int = int -> (int -> int) *)
            TypFun (int_typ, box_node (TypFun (int_typ, int_typ)))
          ]} *)
  | TypTuple of typ node list
      (** Tuple type: [typ1 * ... * typn] where n >= 2.

          {[
            (* SML: int * string *)
            TypTuple [int_typ; string_typ]

            (* SML: int * int * int *)
            TypTuple [int_typ; int_typ; int_typ]
          ]} *)
  | TypRecord of typ_row node list
      (** Record type: [{ typrow }].

          {[
            (* SML: {x : int, y : int} *)
            TypRecord
              [
                box_node (TypRow (x_label, int_typ, None));
                box_node (TypRow (y_label, int_typ, None));
              ]
          ]}

          @see 'typ_row' Type row structure *)

(** Type rows in record types.

    {[
      typrow ::= lab : typ [, typrow]
    ]} *)
and typ_row =
  | TypRow of idx node * typ node * typ_row node option
      (** Type row: [lab : typ].

          {[
            (* SML: x : int (in a record type) *)
            TypRow (box_node (IdxLab (box_node "x")), int_typ, None)
          ]} *)

(** Identifier with optional [op] prefix.

    The [op] keyword removes the infix status of an identifier, allowing it to
    be used as a regular prefix identifier.

    {3 Example}
    {[
      (* SML: op + (use + as prefix) *)
      WithOp (box_node (IdxIdx (box_node "+")))

      (* SML: SOME (normal constructor) *)
      WithoutOp (box_node (IdxIdx (box_node "SOME")))

      (* SML: op :: (use :: as prefix) *)
      WithOp (box_node (IdxIdx (box_node "::")))
    ]}

    @see 'PatIdx' Patterns using with_op
    @see 'FunMatchPrefix' Function clauses using with_op *)
and with_op =
  | WithOp of idx node
      (** Identifier with [op] prefix: [op id].

          Strips infix status, enabling prefix use of infix operators. *)
  | WithoutOp of idx node
      (** Plain identifier without [op] prefix.

          The identifier is used with its normal fixity. *)

(** {2 Patterns}

    {[
      pat ::= constant                               (* constant *)
            | _                                 (* wildcard *)
            | [op] id                           (* variable *)
            | [op] longid [pat]                 (* construction *)
            | pat1 id pat2                      (* infix construction *)
            | ( pat )                           (* parentheses *)
            | ( pat1 , ... , patn )             (* tuple, n != 1 *)
            | { [patrow] }                      (* record *)
            | [ pat1 , ... , patn ]             (* list, n >= 0 *)
            | pat : typ                         (* type annotation *)
            | [op] id [: typ] as pat            (* layered *)
    ]}

    Patterns destructure values and bind variables. They appear in value
    bindings, function arguments, case expressions, and exception handlers.

    @see 'expression' Expressions that patterns match against
    @see 'matching' Match clauses using patterns
    @see 'pat_row' Record pattern rows *)
and pat =
  | PatCon of constant node
      (** Constant pattern: matches a specific constant value.

          {[
            (* SML: 0 *)
            PatCon (box_node (ConInt (box_node "0")))

            (* SML: "hello" *)
            PatCon (box_node (ConString (box_node "hello")))
          ]}

          @see 'constant' Constant types *)
  | PatWildcard
      (** Wildcard pattern: [_]. Matches any value without binding.

          {[
            (* SML: case x of _ => "anything" *)
            PatWildcard
          ]} *)
  | PatIdx of with_op node
      (** Variable or nullary constructor pattern: [[op] id].

          Whether this is a variable binding or constructor match depends on the
          identifier. Constructors are distinguished by capitalization
          conventions (uppercase = constructor).

          {[
            (* SML: x (variable) *)
            PatIdx (box_node (WithoutOp (box_node (IdxIdx (box_node "x")))))

            (* SML: NONE (nullary constructor) *)
            PatIdx (box_node (WithoutOp (box_node (IdxIdx (box_node "NONE")))))
          ]}

          @see 'with_op' Handling of op prefix *)
  | PatApp of with_op node * pat node
      (** Constructor application pattern: [[op] longid pat].

          {[
            (* SML: SOME x *)
            PatApp
              ( box_node (WithoutOp (box_node (IdxIdx (box_node "SOME")))),
                box_node (PatIdx (box_node (WithoutOp x_id))) )
          ]} *)
  | PatInfix of pat node * idx node * pat node
      (** Infix constructor pattern: [pat1 id pat2].

          The identifier must be an infix constructor (e.g., [::]).

          {[
            (* SML: x :: xs *)
            PatInfix (x_pat, box_node (IdxIdx (box_node "::")), xs_pat)

            (* SML: (a, b) :: rest *)
            PatInfix (tuple_pat, cons_id, rest_pat)
          ]} *)
  | PatParen of pat node
      (** Parenthesized pattern: [( pat )].

          Preserved for pretty-printing fidelity. *)
  | PatTuple of pat node list
      (** Tuple pattern: [( pat1 , ... , patn )] where n != 1.

          {[
            (* SML: () *)
            PatTuple []

            (* SML: (x, y) *)
            PatTuple [x_pat; y_pat]

            (* SML: (a, b, c) *)
            PatTuple [a_pat; b_pat; c_pat]
          ]} *)
  | PatRecord of pat_row node list
      (** Record pattern: [{ patrow }].

          {[
            (* SML: {x = a, y = b} *)
            PatRecord [
              box_node (PatRowSimple (x_label, a_pat, empty_row));
              box_node (PatRowSimple (y_label, b_pat, empty_row))
            ]

            (* SML: {x, ...} (punning + wildcard) *)
            PatRecord [
              box_node (PatRowVar (x_label, None, None, Some (box_node PatRowPoly)))
            ]
          ]}

          @see 'pat_row' Pattern row types *)
  | PatList of pat node list
      (** List pattern: [[ pat1 , ... , patn ]] where n >= 0.

          {[
            (* SML: [] *)
            PatList []

            (* SML: [x] *)
            PatList [x_pat]

            (* SML: [a, b, c] *)
            PatList [a_pat; b_pat; c_pat]
          ]} *)
  | PatArray of pat node list
  | PatTyp of pat node * typ node
      (** Type-annotated pattern: [pat : typ].

          {[
            (* SML: (x : int) *)
            PatTyp (x_pat, int_typ)
          ]} *)
  | PatAs of with_op node * typ node option * pat node
      (** Layered (as) pattern: [[op] id [: typ] as pat].

          Binds the identifier to the entire matched value while also matching
          the inner pattern. Useful for accessing both the whole and parts of a
          value.

          {[
            (* SML: xs as (x :: _) *)
            PatAs (
              box_node (WithoutOp (box_node (IdxIdx (box_node "xs")))),
              None,
              box_node (PatInfix (x_pat, cons_id, wildcard_pat))
            )

            (* SML: (node : tree) as Node (left, _, right) *)
            PatAs (
              box_node (WithoutOp node_id),
              Some tree_typ,
              node_app_pat
            )
          ]} *)
  | PatOr of pat node * pat node
      (** Pattern rows in record patterns.

          {[
            patrow ::= ...                                    (* wildcard *)
                     | lab = pat [, patrow]                   (* pattern *)
                     | id [: typ] [as pat] [, patrow]         (* variable *)
          ]}

          @see 'PatRecord' Record patterns using rows *)

and pat_row =
  | PatRowPoly
      (** Wildcard row: [...]. Matches remaining record fields.

          {[
            (* SML: {x = a, ...} - the "..." part *)
            PatRowPoly
          ]} *)
  | PatRowSimple of idx node * pat node * pat_row node
      (** Pattern row: [lab = pat]. Matches a specific field.

          {[
            (* SML: x = a (in a record pattern) *)
            PatRowSimple (x_label, a_pat, empty_row)
          ]} *)
  | PatRowVar of
      idx node * typ node option * idx node option * pat_row node option
      (** Variable row: [id [: typ] [as pat]].

          Shorthand where the label equals the variable name (punning). In SML,
          [{x, y}] is short for [{x = x, y = y}].

          Components:
          - Label/variable name
          - Optional type annotation
          - Optional "as" target (Note: should be pat, stored as idx here)
          - Optional rest of row

          {[
            (* SML: x (punning in record pattern) *)
            PatRowVar (x_id, None, None, None)

            (* SML: x : int *)
            PatRowVar (x_id, Some int_typ, None, None)
          ]} *)

let unwrap_op = function
  | WithOp id -> id
  | WithoutOp id -> id