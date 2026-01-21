# Logger Filtering Improvements

**Date:** 2026-01-21
**Component:** `lib/common/logger.ml`
**Status:** Approved

## Problem Statement

The current logger filtering logic in `lib/common/logger.ml` has several issues:

1. **Debug flag matching is broken**: Uses `starts_with`/`ends_with` logic that doesn't correctly match debug flags to logger groups/subgroups
2. **Confusing code structure**: Misleading variable names (`is_quiet`) and duplicated logic make the code hard to understand
3. **Unclear filtering hierarchy**: The interaction between debug flags, quiet mode, and verbosity levels is not obvious

## Requirements

### Debug Flag Matching
- Debug flag `"backend:conversion"` should match logger with group=`"backend"`, subgroup=`"conversion"` only
- Debug flag `"backend"` should match ALL loggers with group=`"backend"`, regardless of subgroup
- Use exact string matching, not prefix/suffix patterns

### Quiet Mode
- Suppress all messages except errors (kind=`Negative`)
- Applied after debug flag check but before verbosity check

### Debug Flags Bypass Everything
- If a debug flag matches, always print the message
- Ignore both quiet mode and verbosity level restrictions
- Rationale: Explicitly enabling a debug flag indicates user wants those specific messages

## Design

### Filtering Hierarchy

The filtering logic uses a clear three-level priority system:

**Priority 1: Debug flags** (highest)
```
If debug flag matches group/subgroup → Always print
```

**Priority 2: Quiet mode** (medium)
```
If quiet mode enabled AND kind ≠ Negative → Don't print
```

**Priority 3: Verbosity level** (lowest)
```
If verbosity >= level_value → Print
Otherwise → Don't print
```

### Implementation Structure

#### Helper Function: `matches_debug_flag`

Extract debug flag matching into a clear, testable helper:

```ocaml
let matches_debug_flag ~(group: string) ~(subgroup: string) (flag: string) : bool =
  let full_name = if subgroup = "" then group else group ^ ":" ^ subgroup in
  String.equal flag group || String.equal flag full_name
```

**Logic:**
- Match if flag equals just the group name (matches all subgroups)
- OR if flag equals the full "group:subgroup" name

**Examples:**
- `matches_debug_flag ~group:"backend" ~subgroup:"conversion" "backend"` → `true`
- `matches_debug_flag ~group:"backend" ~subgroup:"conversion" "backend:conversion"` → `true`
- `matches_debug_flag ~group:"backend" ~subgroup:"conversion" "backend:other"` → `false`

#### Refactored `get_should_print`

Consolidate all filtering logic into a single, clear function:

```ocaml
let get_should_print ?(subgroup = "") ?(cfg : Options.options = C.config)
    (level : level) (kind : kind) : bool =
  let debug_flags = Options.get_debug cfg in

  (* Priority 1: Debug flags bypass everything *)
  if List.exists (matches_debug_flag ~group:C.group ~subgroup) debug_flags then
    true
  (* Priority 2: Quiet mode suppresses non-errors *)
  else if Options.get_quiet cfg && kind <> Negative then
    false
  (* Priority 3: Verbosity level filtering *)
  else
    let verbosity = Options.get_verbosity_default cfg 0 in
    let level_value = match level with High -> 0 | Medium -> 1 | Low -> 2 | Debug -> 3 in
    verbosity >= level_value
```

**Key improvements:**
- Single source of truth for filtering decisions
- No duplicate debug flag checks
- Clear priority hierarchy with comments
- Early returns make control flow obvious
- Takes `kind` parameter to apply quiet mode logic

#### Simplified `log_with`

With filtering logic consolidated in `get_should_print`, `log_with` becomes much simpler:

```ocaml
let log_with ~cfg ?(subgroup = "") ?(level = High) ?(kind = Negative) ~(msg : string) (() : unit) =
  if get_should_print ~cfg ~subgroup level kind then begin
    let prefix_msg = match kind with
      | Positive -> "SUCCESS "
      | Negative -> "ERROR "
      | Neutral -> "INFO "
      | Warning -> "WARNING "
    in
    let fmt_style = `Fg Fmt.(match kind with
      | Positive -> `Green
      | Negative -> `Red
      | Neutral -> `Blue
      | Warning -> `Yellow)
    in
    let format_bold = Fmt.(styled `Bold string) in
    Fmt.epr "%a" Fmt.(styled fmt_style format_bold) prefix_msg;
    Fmt.epr "%s@." msg
  end
```

**Simplifications:**
- All filtering moved to `get_should_print`
- Removed confusing `is_quiet` variable
- Removed duplicate debug flag check
- Removed duplicate verbosity calculation
- Single responsibility: format and print if allowed

## Backward Compatibility

- No changes to module signatures (`LOG`, `S`)
- No changes to public API (`log`, `log_with` function signatures)
- Existing calling code requires no modifications
- Only internal implementation changes

## Testing Considerations

The new `matches_debug_flag` helper function is independently testable. Test cases should cover:

1. Exact group:subgroup matching
2. Group-only matching (all subgroups)
3. Non-matching cases
4. Empty subgroup handling

Integration testing should verify:
- Debug flags bypass quiet mode
- Debug flags bypass verbosity restrictions
- Quiet mode only shows errors
- Verbosity levels work correctly when not in quiet mode

## Migration Path

1. Implement `matches_debug_flag` helper function
2. Refactor `get_should_print` to use new logic and take `kind` parameter
3. Simplify `log_with` to use refactored `get_should_print`
4. Update `log` function signature if needed (currently just calls `log_with`)
5. Run existing tests to verify backward compatibility
6. Add unit tests for new `matches_debug_flag` function
