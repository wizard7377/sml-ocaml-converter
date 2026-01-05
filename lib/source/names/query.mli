(** Query language for filtering name stores.

    {1 Synopsis}

    This module provides a compositional query language for selecting
    entries from name stores. Queries can be combined with logical
    operations (AND, OR, NOT, XOR) and filter on various name attributes.

    {1 Overview}

    The query system allows building complex predicates for filtering
    {!Store.entry} values. Queries are first-class values that can be
    composed and reused.

    {1 Basic Queries}

    Atomic queries filter on specific name attributes:
    - {!name_query}: Filter by root name string
    - {!context_query}: Filter by syntactic context
    - {!path_query}: Filter by module path
    - {!package_query}: Filter by package specifier
    - {!action_query}: Filter by transformation action

    {1 Logical Combinators}

    Queries can be combined using logical operations:
    - {!query_and}: Both queries must succeed
    - {!query_or}: At least one query must succeed
    - {!query_not}: Invert query result
    - {!query_xor}: Exactly one query must succeed

    {1 Usage Example}

    {[
      (* Find all constructors in the Foo module *)
      let q = query_and
        (context_query (fun c -> c = Store.Constructor))
        (path_query (fun p -> p = ["Foo"]))
      in
      let (matching, non_matching) = select_query ~query:q ~store:my_store in

      (* Find all types or constructors *)
      let q2 = query_or
        (context_query (fun c -> c = Store.Type))
        (context_query (fun c -> c = Store.Constructor))
      in

      (* Find names that need exact renaming *)
      let q3 = action_query (function Store.Exact _ -> true | _ -> false) in
    ]}

    {1 Type Definitions} *)

(** Abstract type representing a query predicate.

    Queries are composable and can be evaluated against store entries
    to determine if they match. *)
type t

(** {1 Query Execution} *)

(** [run_query ~query ~entry] tests whether an entry matches a query.

    @param query The query predicate to test
    @param entry The store entry to test against
    @return [true] if the entry satisfies the query, [false] otherwise *)
val run_query : query:t -> entry:Store.entry -> bool

(** [select_query ~query ~store] partitions a store by a query.

    This is a convenience wrapper around {!run_query} that filters
    an entire store.

    @param query The query predicate to apply
    @param store The store to partition
    @return A pair of [(matching, non_matching)] stores *)
val select_query : query:t -> store:Store.t -> Store.t * Store.t

(** {1 Logical Combinators} *)

(** [query_and q1 q2] creates a conjunction of two queries.

    The resulting query succeeds only if both [q1] and [q2] succeed.

    @param q1 First query
    @param q2 Second query
    @return Query that represents [q1 AND q2] *)
val query_and : t -> t -> t

(** [query_or q1 q2] creates a disjunction of two queries.

    The resulting query succeeds if either [q1] or [q2] succeeds.

    @param q1 First query
    @param q2 Second query
    @return Query that represents [q1 OR q2] *)
val query_or : t -> t -> t

(** [query_not q] creates a negation of a query.

    The resulting query succeeds if [q] fails.

    @param q Query to negate
    @return Query that represents [NOT q] *)
val query_not : t -> t

(** [query_xor q1 q2] creates an exclusive-or of two queries.

    The resulting query succeeds if exactly one of [q1] or [q2] succeeds.

    @param q1 First query
    @param q2 Second query
    @return Query that represents [q1 XOR q2] *)
val query_xor : t -> t -> t

(** {1 Atomic Queries}

    These functions create basic queries that filter on specific
    name attributes. Each takes a predicate function that tests
    the corresponding attribute. *)

(** [name_query pred] filters entries by their root name.

    Example: [name_query ((=) "x")] matches entries named "x"

    @param pred Predicate function testing the root name string
    @return Query that succeeds if [pred root_name] is true *)
val name_query : (Name.name -> bool) -> t

(** [context_query pred] filters entries by their syntactic context.

    Example: [context_query ((=) Store.Constructor)] matches constructors

    @param pred Predicate function testing the name context
    @return Query that succeeds if [pred context] is true *)
val context_query : (Store.name_context -> bool) -> t

(** [path_query pred] filters entries by their module path.

    Example: [path_query (fun p -> List.length p > 0)] matches qualified names

    @param pred Predicate function testing the module path
    @return Query that succeeds if [pred path] is true *)
val path_query : (Name.path -> bool) -> t

(** [package_query pred] filters entries by their package specifier.

    Example: [package_query ((=) Name.Global)] matches global names

    @param pred Predicate function testing the package
    @return Query that succeeds if [pred package] is true *)
val package_query : (Name.package -> bool) -> t

(** [action_query pred] filters entries by their transformation action.

    Example: [action_query ((=) Store.NoChange)] matches unchanged names

    @param pred Predicate function testing the action
    @return Query that succeeds if [pred action] is true *)
val action_query : (Store.action -> bool) -> t

(** {1 Constant Queries} *)

(** [yes_query] is a query that always succeeds.

    Useful as a base case for building more complex queries.
    Example: [query_and yes_query q] is equivalent to [q] *)
val yes_query : t

(** [no_query] is a query that always fails.

    Useful for filtering out all entries.
    Example: [query_or no_query q] is equivalent to [q] *)
val no_query : t
