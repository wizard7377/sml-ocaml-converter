# Backend Stub Functions

This document lists all the stub functions created in `lib/backend/backend.ml` with `assert false` implementations. These serve as placeholders for future implementation.

## Summary

**Total stub functions created**: 34

All functions follow proper pattern matching on their input AST types and have appropriate return types based on the OCaml Parsetree structure.

## Pattern Processing (11 functions)

### `process_pat : Ast.pat -> Parsetree.pattern`
Converts SML patterns to OCaml patterns. Handles all pattern forms:
- Constant patterns (`PatCon`)
- Wildcard (`PatWildcard`)
- Identifiers (`PatIdx`)
- Constructor application (`PatApp`)
- Infix constructors (`PatInfix`)
- Parenthesized patterns (`PatParen`)
- Tuples (`PatTuple`)
- Records (`PatRecord`)
- Lists (`PatList`)
- Type annotations (`PatTyp`)
- Layered/as patterns (`PatAs`)

### `process_pat_row : Ast.pat_row -> (string * Parsetree.pattern) list`
Converts SML pattern rows (record patterns) to OCaml equivalents.
- Polymorphic row (`PatRowPoly`)
- Simple field pattern (`PatRowSimple`)
- Variable shorthand (`PatRowVar`)

## Expression Processing (2 functions)

### `process_row : Ast.row -> (Longident.t Location.loc * Parsetree.expression)`
Converts SML expression rows (record fields) to OCaml.

### `process_matching : Ast.matching -> Parsetree.case list`
Converts SML match clauses to OCaml case expressions.

## Declaration Processing (10 functions)

### `process_dec : Ast.declaration -> Parsetree.value_binding list`
Converts SML declarations to OCaml value bindings. Handles:
- Value declarations (`ValDec`)
- Function declarations (`FunDec`)
- Type declarations (`TypDec`)
- Datatype declarations (`DatDec`)
- Datatype aliases (`DataDecAlias`)
- Abstract types (`AbstractDec`)
- Exception declarations (`ExnDec`)
- Structure declarations (`StrDec`)
- Sequential declarations (`SeqDec`)
- Local declarations (`LocalDec`)
- Open declarations (`OpenDec`)
- Fixity declarations (`FixityDec`)

### `process_fixity : Ast.fixity -> string`
Converts SML fixity specifications.

### `process_val_bind : Ast.value_binding -> Parsetree.value_binding list`
Converts SML value bindings (pattern = expression).

### `process_fun_bind : Ast.function_binding -> Parsetree.value_binding list`
Converts SML function bindings.

### `process_fun_match : Ast.fun_match -> (Parsetree.pattern list * Parsetree.expression) list`
Converts SML function match clauses.

### `process_typ_bind : Ast.type_binding -> Parsetree.type_declaration list`
Converts SML type abbreviations.

### `process_dat_bind : Ast.data_binding -> Parsetree.type_declaration list`
Converts SML datatype declarations.

### `process_con_bind : Ast.constructor_binding -> Parsetree.constructor_declaration list`
Converts SML constructor bindings.

### `process_exn_bind : Ast.exn_bind -> Parsetree.extension_constructor list`
Converts SML exception bindings.

### `process_with_op : Ast.with_op -> idx`
Extracts identifier from optional `op` prefix.

## Structure Processing (3 functions)

### `process_str : Ast.structure -> Parsetree.structure_item list`
Converts SML structures to OCaml structures. Handles:
- Structure identifiers (`StrIdx`)
- Structure expressions (`StructStr`)
- Annotated structures (`AnotateStr`)
- Functor applications (`FunctorApp`, `FunctorAppAnonymous`)
- Local declarations (`LocalDec`)

### `process_anotate : Ast.anotate -> string`
Converts signature annotations (transparent/opaque).

### `process_str_bind : Ast.structure_binding -> Parsetree.module_binding list`
Converts SML structure bindings.

## Signature Processing (9 functions)

### `process_sign : Ast.signature -> Parsetree.signature_item list`
Converts SML signatures to OCaml signatures.

### `process_typ_refine : Ast.typ_refine -> (Longident.t * Parsetree.core_type) list`
Converts SML type refinements (where clauses).

### `process_spec : Ast.specification -> Parsetree.signature_item list`
Converts SML specifications to OCaml signature items. Handles:
- Value specifications (`SpecVal`)
- Type specifications (`SpecTyp`, `SpecEqtyp`)
- Type bindings in signatures (`SpecTypBind`)
- Datatype specifications (`SpecDat`, `SpecDatAlias`)
- Exception specifications (`SpecExn`)
- Structure specifications (`SpecStr`)
- Sequential specs (`SpecSeq`)
- Include directives (`SpecInclude`, `SpecIncludeIdx`)
- Sharing constraints (`SpecSharingTyp`, `SpecSharingStr`)

### `process_val_specification : Ast.val_specification -> Parsetree.value_description list`
Converts SML value descriptions in signatures.

### `process_typ_specification : Ast.typ_specification -> Parsetree.type_declaration list`
Converts SML abstract type descriptions.

### `process_dat_specification : Ast.dat_specification -> Parsetree.type_declaration list`
Converts SML datatype descriptions in signatures.

### `process_con_specification : Ast.con_specification -> Parsetree.constructor_declaration list`
Converts SML constructor descriptions.

### `process_exn_specification : Ast.exn_specification -> Parsetree.extension_constructor list`
Converts SML exception descriptions.

### `process_str_specification : Ast.str_specification -> Parsetree.module_declaration list`
Converts SML structure descriptions in signatures.

## Program Processing (3 functions)

### `process_prog : Ast.prog -> Parsetree.structure`
Converts top-level SML programs to OCaml structures. Handles:
- Core declarations (`ProgDec`)
- Functor declarations (`ProgFun`)
- Signature declarations (`ProgStr`)
- Sequential programs (`ProgSeq`)
- Empty programs (`ProgEmpty`)

### `process_functor_binding : Ast.functor_binding -> Parsetree.module_binding list`
Converts SML functor bindings.

### `process_signature_binding : Ast.signature_binding -> Parsetree.module_type_declaration list`
Converts SML signature bindings.

## Implementation Status

- ✅ **Type processing**: Implemented (`process_type_value`, `process_object_field_type`)
- ⚠️ **Constant processing**: Stub only (`process_con`)
- ⚠️ **Expression processing**: Partially implemented (`process_exp`)
- ⚠️ **Pattern processing**: Stub only (all pattern functions)
- ⚠️ **Declaration processing**: Stub only (all declaration functions)
- ⚠️ **Structure processing**: Stub only (all structure functions)
- ⚠️ **Signature processing**: Stub only (all signature functions)
- ⚠️ **Program processing**: Stub only (all program functions)

## Notes

- All stub functions compile without errors
- All functions have proper type signatures
- Functions are organized into logical sections with documentation headers
- Pattern matching is exhaustive for all AST constructors
- Warning 27 (unused variables) is expected for stub functions and will disappear as implementations are added
