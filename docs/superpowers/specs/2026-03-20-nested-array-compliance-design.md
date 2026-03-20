# TOML v1.0.0 Nested Array & Dotted Key Compliance

**Date:** 2026-03-20
**Status:** Draft
**Scope:** Full TOML v1.0.0 compliance for nested keys, arrays of tables, and dotted key-value pairs

## Problem

The serializer emits `[[key]]` headers using only the immediate key name instead of the full path from root. Nested arrays of tables produce `[[varieties]]` instead of `[[fruits.varieties]]`. Sub-tables within array elements also lose their path context.

The parser stores dotted keys in key-value pairs (e.g., `physical.color = "red"`) as flat strings (`"physical.color"`) instead of creating nested table structures (`{"physical" -> Table{"color" -> "red"}}`). This contradicts the TOML v1.0.0 spec and creates an inconsistency with `[table]` header parsing, which correctly creates nested structures. The same bug affects dotted keys inside inline tables, since `ParseInlineTable` delegates to `ParseKeyValue`.

## Approach

Fix both parser and serializer. Update all tests to assert spec-correct behavior. No compatibility flags -- the correct spec behavior is the contract.

## Already Working (No Changes Needed)

The following TOML v1.0.0 features are already correctly handled and are not affected by this change:

- **Dotted keys in `[table]` and `[[array]]` headers** -- The `Parse` method collects path parts into `TablePath` and navigates the tree correctly for both `[a.b]` and `[[a.b]]`.
- **Super-tables defined after sub-tables** -- e.g., `[a.b]` followed by `[a]`. The `Parse` method checks `TryGetValue` first and reuses existing tables. This code path does not call `TTOMLTable.Add`, so the merge logic change in Section 2 does not affect it.
- **Intermediate array path resolution** -- When navigating `[[a.b]]` where `a` is an array-of-tables, the parser resolves to the last element of `a` per the TOML spec.

## Changes

### 1. Parser: Dotted Keys in Key-Value Pairs (TOML.Parser.pas)

**Method:** `ParseKeyValue`

Current behavior concatenates dotted key parts into a flat string:
```pascal
Key := ParseKey;
while Match(ttDot) do
  Key := Key + '.' + ParseKey;
```

Change to collect key parts into a list, then wrap the parsed value in nested `TTOMLTable` instances from innermost to outermost:

- Parse `physical.color = "red"`
- Collect parts: `["physical", "color"]`
- Build: `TTOMLTable{"color" -> TTOMLString("red")}`
- Return key `"physical"`, value is the wrapping table

For a three-part key like `a.b.c = 1`:
- Collect parts: `["a", "b", "c"]`
- Build: `TTOMLTable{"b" -> TTOMLTable{"c" -> TTOMLInteger(1)}}`
- Return key `"a"`, value is the outermost wrapping table

**Inline tables:** This fix automatically covers dotted keys inside inline tables as well, since `ParseInlineTable` delegates to `ParseKeyValue`.

### 2. Parser: Implicit Table Merging (TOML.Types.pas)

**Method:** `TTOMLTable.Add`

When two dotted keys share a prefix:
```toml
physical.color = "red"
physical.shape = "round"
```

The first creates `{"physical" -> Table{color: "red"}}`. The second must merge into the existing `physical` table.

Change `TTOMLTable.Add`: when the key already exists, if both the existing value and the new value are `TTOMLTable`, merge the new table's entries into the existing one by iterating the new table's entries and calling `Add` for each one. This recursive `Add` call ensures:
- Nested shared prefixes are merged correctly at all levels.
- Duplicate leaf keys (e.g., `a.b.c = 1` followed by `a.b.c = 2`) still raise a duplicate key error, because the innermost `Add` finds a non-table conflict.

**Memory ownership:** After merging, `Add` must free the now-empty wrapper table that was passed in. The caller (`Parse`) must not attempt to free the value after a successful `Add` call. On exception, the caller's existing `except` block (which calls `KeyPair.Value.Free`) remains correct because the merge has not yet consumed the value -- `Add` should only free the wrapper after all entries have been successfully merged. If any entry fails to merge (e.g., duplicate key), `Add` should raise before freeing, leaving the wrapper intact for the caller to free.

### 3. Serializer: Full Path for Array-of-Tables Headers (TOML.Serializer.pas)

**Method:** `WriteTable`

Extract a shared helper:
```pascal
function TTOMLSerializer.BuildPath(const AKey: string): string;
```

This builds the full dotted path from `FCurrentPath` components plus the given key, quoting any component that `NeedsQuoting` returns true for, joined by dots. When `AKey` is empty, it builds the path from `FCurrentPath` alone (no trailing dot).

Replace the hardcoded `'[[' + Pair.Key + ']]'` with `'[[' + BuildPath(Pair.Key) + ']]'`.

Replace the duplicated path-building code in the regular `[table]` section (lines 418-446) with a call to `BuildPath` as well.

### 4. Serializer: Path Context for Array Element Recursion (TOML.Serializer.pas)

**Method:** `WriteTable`

When recursing into array-of-table elements, use approach (b): build the header first via `BuildPath(Pair.Key)`, then push the key for recursion:

```pascal
for i := 0 to ArrayValue.Count - 1 do
begin
  if i > 0 then
    WriteLine;
  WriteLine('[[' + BuildPath(Pair.Key) + ']]');
  FCurrentPath.Add(Pair.Key);
  WriteTable(ArrayValue.GetItem(i).AsTable);
  FCurrentPath.Delete(FCurrentPath.Count - 1);
end;
```

This pattern mirrors the existing sub-table code: the header uses `BuildPath` with the key, then the key is pushed for recursion so nested structures see the correct context. The push/pop is inside the loop so each array element gets the same path context.

Once the path context is correct, nested arrays-of-tables and sub-tables within array elements automatically produce correct full paths through recursion.

### 5. Test Updates (TestCaseTOML.pas)

**Fix existing tests:**

- `Test68_KeyValidation`: Change assertions for `valid.key = "value"` from flat `TryGetValue('valid.key', ...)` to nested navigation: `TryGetValue('valid', ...) -> .AsTable.TryGetValue('key', ...)`. Same for `valid."dotted.key"` which should produce `{"valid" -> Table{"dotted.key" -> "value"}}`.

**Add new tests:**

1. **Serializer: nested array-of-tables** -- Build a `fruits` array where elements contain a `varieties` array of tables. Serialize and assert `[[fruits.varieties]]` appears. Parse back and verify structure.

2. **Serializer: sub-tables under array elements** -- Build a `fruits` array where elements contain a `physical` sub-table. Serialize and assert `[fruits.physical]` appears.

3. **Serializer: deep nesting** -- Build `a -> array[b -> array[c -> array[...]]]`. Serialize and verify `[[a]]`, `[[a.b]]`, `[[a.b.c]]` all appear.

4. **Parser: dotted key creates nested tables** -- Parse `physical.color = "red"`, assert nested table structure.

5. **Parser: dotted key merging** -- Parse `physical.color = "red"` + `physical.shape = "round"`, assert both in same `physical` table.

6. **Parser: duplicate dotted key error** -- Parse `physical.color = "red"` + `physical.color = "blue"`, assert duplicate key error is raised.

7. **Parser: dotted keys in inline tables** -- Parse `point = { x.y = 1, x.z = 2 }`, assert nested structure with merged `x` table.

8. **Round-trip: TOML spec canonical example** -- The `[[fruits]]` / `[fruits.physical]` / `[[fruits.varieties]]` example. Parse, serialize, parse again, verify structural equality.

9. **Round-trip: deep `[[a.b.c]]` chains** -- Parse deep nesting, serialize back, verify output matches.

## Files Modified

| File | Change |
|------|--------|
| `src/TOML.Parser.pas` | `ParseKeyValue`: collect key parts, wrap value in nested tables |
| `src/TOML.Types.pas` | `TTOMLTable.Add`: merge when both existing and new are tables |
| `src/TOML.Serializer.pas` | Add `BuildPath` helper; fix `[[...]]` headers; fix `FCurrentPath` tracking for array element recursion |
| `tests/TestCaseTOML.pas` | Fix Test68; add 9 new tests |

## Not in Scope

- Performance optimization for large files
- TOML features unrelated to nesting (multiline strings, number formats, etc.)
- Delphi-specific serialization testing (separate effort)
- Line-ending normalization of the repository
