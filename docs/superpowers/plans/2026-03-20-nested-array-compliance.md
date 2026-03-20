# TOML v1.0.0 Nested Array & Dotted Key Compliance — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Achieve full TOML v1.0.0 compliance for nested arrays of tables, sub-tables within array elements, and dotted keys in key-value pairs.

**Architecture:** Fix the parser to create nested table structures for dotted keys (instead of flat string concatenation) and add implicit table merging. Fix the serializer to track full paths when emitting `[[array]]` headers and recursing into array element tables. Update all tests to assert spec-correct behavior.

**Tech Stack:** Object Pascal (Delphi 12 + Free Pascal). Source in `src/`, tests in `tests/` using FPC's fpcunit. Compile source units via delphi-build MCP (`mcp__delphi-build__compile_delphi_project`). Test runner requires FPC/lazbuild (`.lpi` project).

**Spec:** `docs/superpowers/specs/2026-03-20-nested-array-compliance-design.md`

**Build note:** The tests use FPC's `consoletestrunner` and `fpcunit` framework (`tests/TestRunner.lpr`). After each code change, verify the source compiles in Delphi via the delphi-build MCP using the package project. Full test execution requires FPC/lazbuild on the test runner project.

**Delphi style:** Follow `delphi-style` skill for all code changes. Use English comments, XML documentation on new methods, PascalCase for types, camelCase for locals.

---

## File Map

| File | Action | Responsibility |
|------|--------|----------------|
| `src/TOML.Types.pas:417-429` | Modify | `TTOMLTable.Add` — add implicit table merge logic |
| `src/TOML.Parser.pas:1335-1354` | Modify | `ParseKeyValue` — collect dotted key parts, wrap value in nested tables |
| `src/TOML.Serializer.pas:44-108` | Modify | Add `BuildPath` to class declaration |
| `src/TOML.Serializer.pas:328-461` | Modify | `WriteTable` — use `BuildPath`, fix `FCurrentPath` for array-of-tables |
| `tests/TestCaseTOML.pas:11-85` | Modify | Add new test method declarations |
| `tests/TestCaseTOML.pas:1200-1232` | Modify | Fix `Test68_KeyValidation` assertions |
| `tests/TestCaseTOML.pas` (append) | Modify | Add 9 new test procedures |

---

### Task 1: Implicit Table Merging in TTOMLTable.Add

**Files:**
- Modify: `src/TOML.Types.pas:417-429`

This is the foundation — later parser changes depend on merge working correctly.

- [ ] **Step 1: Read current `TTOMLTable.Add` implementation**

Read `src/TOML.Types.pas:417-429`. Current code:
```pascal
procedure TTOMLTable.Add(const AKey: string; AValue: TTOMLValue);
var
  ExistingValue: TTOMLValue;
begin
  if FItems = nil then
    FItems := TTOMLTableDict.Create;
  if FItems.TryGetValue(AKey, ExistingValue) then
    raise ETOMLParserException.CreateFmt('Duplicate key "%s" found', [AKey]);
  FItems.AddOrSetValue(AKey, AValue);
end;
```

- [ ] **Step 2: Add `Classes` to uses clause**

In `src/TOML.Types.pas`, line 26, change:
```pascal
uses
  SysUtils, Generics.Collections;
```
to:
```pascal
uses
  SysUtils, Classes, Generics.Collections;
```

This is needed for `TStringList` used in the merge logic below.

- [ ] **Step 3: Implement merge logic**

Replace the body of `TTOMLTable.Add` with:
```pascal
procedure TTOMLTable.Add(const AKey: string; AValue: TTOMLValue);
var
  ExistingValue: TTOMLValue;
  MergeSource: TTOMLTable;
  MergeKeys: TStringList;
  MergeKey: string;
  MergeValue: TTOMLValue;
  i: Integer;
begin
  if FItems = nil then
    FItems := TTOMLTableDict.Create;

  if FItems.TryGetValue(AKey, ExistingValue) then
  begin
    // Both existing and new are tables: merge new entries into existing
    if (ExistingValue is TTOMLTable) and (AValue is TTOMLTable) then
    begin
      MergeSource := TTOMLTable(AValue);
      // Collect keys first to avoid iterator invalidation
      MergeKeys := TStringList.Create;
      try
        for MergeKey in MergeSource.Items.Keys do
          MergeKeys.Add(MergeKey);
        // Transfer each entry: detach from source before adding to target
        // This prevents double-free if a later Add raises on duplicate key
        for i := 0 to MergeKeys.Count - 1 do
        begin
          MergeKey := MergeKeys[i];
          MergeValue := MergeSource.Items[MergeKey];
          // Detach from source BEFORE adding to target
          MergeSource.Items.Remove(MergeKey);
          try
            TTOMLTable(ExistingValue).Add(MergeKey, MergeValue);
          except
            // Re-attach to source so caller's except block can free it
            MergeSource.Items.AddOrSetValue(MergeKey, MergeValue);
            raise;
          end;
        end;
      finally
        MergeKeys.Free;
      end;
      // All entries transferred — free the now-empty wrapper
      MergeSource.Free;
    end
    else
      raise ETOMLParserException.CreateFmt('Duplicate key "%s" found', [AKey]);
  end
  else
    FItems.AddOrSetValue(AKey, AValue);
end;
```

Key points:
- Collects keys into a `TStringList` first to avoid dictionary iterator invalidation.
- Each entry is detached from `MergeSource.Items.Remove` BEFORE calling `Add` on the target. This prevents double-free: the value is owned by exactly one dictionary at any time.
- If `Add` raises (duplicate leaf key), the entry is re-attached to `MergeSource` so the caller's `except` block (`KeyPair.Value.Free`) can properly free the entire wrapper chain.
- After all entries transfer successfully, the now-empty wrapper is freed.
- Uses `TStringList` for key iteration (via `Classes` unit added in Step 2) instead of `TPair` — avoids FPC/Delphi enumerator type mismatch issues.

- [ ] **Step 4: Verify Delphi compilation**

Run: `mcp__delphi-build__compile_delphi_project` with `project_path` = Windows path to `packages/lazarus/toml_fp.lpk` or the main source. Since this is a package, compile one of the example `.dpr` projects or use the package `.dpk` if available.

Note: If no `.dpr`/`.dproj` exists for just the library, create a minimal test compile by using one of the example projects that includes all source units.

Expected: No compilation errors.

- [ ] **Step 5: Commit**

```bash
git add src/TOML.Types.pas
git commit -m "Add implicit table merging to TTOMLTable.Add for dotted key support"
```

---

### Task 2: Parser — Dotted Keys Create Nested Tables

**Files:**
- Modify: `src/TOML.Parser.pas:1335-1354`

Depends on: Task 1 (merge logic must be in place for shared-prefix dotted keys).

- [ ] **Step 1: Read current `ParseKeyValue` implementation**

Read `src/TOML.Parser.pas:1335-1354`. Current code concatenates dotted parts into flat string.

- [ ] **Step 2: Implement nested table wrapping**

Replace `ParseKeyValue` with:
```pascal
function TTOMLParser.ParseKeyValue: TTOMLKeyValuePair;
var
  KeyParts: TStringList;
  Value: TTOMLValue;
  WrapperTable: TTOMLTable;
  i: Integer;
begin
  KeyParts := TStringList.Create;
  try
    KeyParts.Add(ParseKey);
    Value := nil;

    try
      while Match(ttDot) do
        KeyParts.Add(ParseKey);

      Expect(ttEqual);
      Value := ParseValue;

      // Wrap value in nested tables from innermost to outermost
      // For parts ["a", "b", "c"] with value V:
      //   i=1 (from count-2 downto 1): wrap in Table{"c" -> V}, then Table{"b" -> ...}
      // Result key = "a", Result value = outermost wrapper
      for i := KeyParts.Count - 1 downto 1 do
      begin
        WrapperTable := TTOMLTable.Create;
        WrapperTable.Add(KeyParts[i], Value);
        Value := WrapperTable;
      end;

      Result := TTOMLKeyValuePair.Create(KeyParts[0], Value);
    except
      Value.Free;
      raise;
    end;
  finally
    KeyParts.Free;
  end;
end;
```

Key points:
- Single-part keys (no dots) skip the wrapping loop entirely — `KeyParts.Count - 1 downto 1` does nothing when count is 1.
- Multi-part keys wrap from innermost to outermost.
- On exception, the partially-built value chain is freed via `Value.Free` (each wrapper table's destructor frees its children).
- Inline tables are automatically covered since `ParseInlineTable` calls `ParseKeyValue`.

- [ ] **Step 3: Verify Delphi compilation**

Run: `mcp__delphi-build__compile_delphi_project` on an example project.
Expected: No compilation errors.

- [ ] **Step 4: Commit**

```bash
git add src/TOML.Parser.pas
git commit -m "Fix ParseKeyValue to create nested tables for dotted keys per TOML v1.0.0"
```

---

### Task 3: Serializer — BuildPath Helper and Array-of-Tables Fix

**Files:**
- Modify: `src/TOML.Serializer.pas:44-108` (class declaration)
- Modify: `src/TOML.Serializer.pas:328-461` (WriteTable implementation)

- [ ] **Step 1: Add `BuildPath` to class declaration**

In `src/TOML.Serializer.pas`, add after the `NeedsQuoting` declaration (line 95):
```pascal
    { Builds a full dotted path from FCurrentPath components plus an optional key.
      @param AKey Optional key to append. When empty, builds from FCurrentPath alone.
      @returns The full dotted path with proper quoting }
    function BuildPath(const AKey: string = ''): string;
```

- [ ] **Step 2: Implement `BuildPath`**

Add the implementation after the `NeedsQuoting` function (after line 218):
```pascal
function TTOMLSerializer.BuildPath(const AKey: string = ''): string;
var
  i: Integer;
  Component: string;
begin
  Result := '';
  for i := 0 to FCurrentPath.Count - 1 do
  begin
    if i > 0 then
      Result := Result + '.';
    Component := FCurrentPath[i];
    if NeedsQuoting(Component) then
      Result := Result + '"' + Component + '"'
    else
      Result := Result + Component;
  end;
  if AKey <> '' then
  begin
    if Result <> '' then
      Result := Result + '.';
    if NeedsQuoting(AKey) then
      Result := Result + '"' + AKey + '"'
    else
      Result := Result + AKey;
  end;
end;
```

- [ ] **Step 3: Rewrite `WriteTable` — array-of-tables section**

In `WriteTable`, replace the array-of-tables block (lines 395-408) with:
```pascal
        if AllTables then
        begin
          for i := 0 to ArrayValue.Count - 1 do
          begin
            if i > 0 then
              WriteLine;
            WriteLine('[[' + BuildPath(Pair.Key) + ']]');
            FCurrentPath.Add(Pair.Key);
            WriteTable(ArrayValue.GetItem(i).AsTable);
            FCurrentPath.Delete(FCurrentPath.Count - 1);
          end;
          continue;
        end;
```

Changes from original:
- `BuildPath(Pair.Key)` instead of just `Pair.Key` — produces full path.
- `FCurrentPath.Add`/`Delete` around recursion — sub-tables and nested arrays within elements see correct context.

- [ ] **Step 4: Rewrite `WriteTable` — regular sub-table section**

Replace the sub-table block (lines 411-458) with simplified version using `BuildPath`:
```pascal
      if Pair.Value.ValueType = tvtTable then
      begin
        SubTable := Pair.Value.AsTable;
        WriteLine;
        WriteLine('[' + BuildPath(Pair.Key) + ']');
        if SubTable.Items.Count > 0 then
        begin
          FCurrentPath.Add(Pair.Key);
          WriteTable(SubTable);
          FCurrentPath.Delete(FCurrentPath.Count - 1);
        end;
      end;
```

This replaces the manual `PathComponents` TStringList construction (lines 419-457) with a single `BuildPath` call. The `FCurrentPath.Add`/`Delete` pattern is unchanged.

- [ ] **Step 5: Verify Delphi compilation**

Run: `mcp__delphi-build__compile_delphi_project` on an example project.
Expected: No compilation errors.

- [ ] **Step 6: Commit**

```bash
git add src/TOML.Serializer.pas
git commit -m "Fix serializer path tracking for nested arrays-of-tables and sub-tables"
```

---

### Task 4: Fix Existing Test — Test68_KeyValidation

**Files:**
- Modify: `tests/TestCaseTOML.pas:1200-1232`

- [ ] **Step 1: Read current Test68 implementation**

Read `tests/TestCaseTOML.pas:1200-1232`.

- [ ] **Step 2: Fix Test 1 (dotted key) assertions**

Replace the Test 1 block (lines 1205-1217):
```pascal
  // Test 1: Dotted key creates nested tables
  try
    Data := ParseTOML('valid.key = "value"');
    try
      AssertTrue('valid table exists', Data.TryGetValue('valid', Value));
      AssertTrue('key exists in valid table', Value.AsTable.TryGetValue('key', Value));
      AssertEquals('key value', 'value', Value.AsString);
    finally
      Data.Free;
    end;
  except
    on E: ETOMLParserException do
      Fail('Valid dotted key should be allowed');
  end;
```

- [ ] **Step 3: Fix Test 2 (quoted dotted key) assertions**

Replace the Test 2 block (lines 1219-1232):
```pascal
  // Test 2: Quoted key with dots creates nested table with literal dotted key
  try
    Data := ParseTOML('valid."dotted.key" = "value"');
    try
      AssertTrue('valid table exists', Data.TryGetValue('valid', Value));
      AssertTrue('"dotted.key" exists in valid table',
        Value.AsTable.TryGetValue('dotted.key', Value));
      AssertEquals('"dotted.key" value', 'value', Value.AsString);
    finally
      Data.Free;
    end;
  except
    on E: ETOMLParserException do
      Fail('Quoted key containing dots should be allowed');
  end;
```

- [ ] **Step 4: Commit**

```bash
git add tests/TestCaseTOML.pas
git commit -m "Fix Test68_KeyValidation to assert nested table structure for dotted keys"
```

---

### Task 5: New Parser Tests

**Files:**
- Modify: `tests/TestCaseTOML.pas:11-85` (add declarations)
- Modify: `tests/TestCaseTOML.pas` (append implementations)

- [ ] **Step 1: Add test declarations**

In the class declaration section (after line 84, before `end;`), add:
```pascal
    { Nested key compliance tests }
    procedure Test73_DottedKeyCreatesNestedTables;
    procedure Test74_DottedKeyMerging;
    procedure Test75_DuplicateDottedKeyError;
    procedure Test76_DottedKeysInInlineTables;
```

- [ ] **Step 2: Implement Test73 — dotted key creates nested tables**

```pascal
procedure TTOMLTestCase.Test73_DottedKeyCreatesNestedTables;
var
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  Doc := ParseTOML('physical.color = "red"');
  try
    AssertTrue('physical table exists', Doc.TryGetValue('physical', Value));
    AssertEquals('physical is a table', Ord(tvtTable), Ord(Value.ValueType));
    AssertTrue('color exists', Value.AsTable.TryGetValue('color', Value));
    AssertEquals('color value', 'red', Value.AsString);
  finally
    Doc.Free;
  end;

  // Three-part dotted key
  Doc := ParseTOML('a.b.c = 42');
  try
    AssertTrue('a exists', Doc.TryGetValue('a', Value));
    AssertTrue('b exists', Value.AsTable.TryGetValue('b', Value));
    AssertTrue('c exists', Value.AsTable.TryGetValue('c', Value));
    AssertEquals('c value', 42, Value.AsInteger);
  finally
    Doc.Free;
  end;
end;
```

- [ ] **Step 3: Implement Test74 — dotted key merging**

```pascal
procedure TTOMLTestCase.Test74_DottedKeyMerging;
var
  Doc: TTOMLTable;
  Value, PhysicalValue: TTOMLValue;
begin
  Doc := ParseTOML(
    'physical.color = "red"' + LineEnding +
    'physical.shape = "round"'
  );
  try
    AssertTrue('physical exists', Doc.TryGetValue('physical', PhysicalValue));
    AssertTrue('color exists', PhysicalValue.AsTable.TryGetValue('color', Value));
    AssertEquals('color', 'red', Value.AsString);
    AssertTrue('shape exists', PhysicalValue.AsTable.TryGetValue('shape', Value));
    AssertEquals('shape', 'round', Value.AsString);
  finally
    Doc.Free;
  end;

  // Three-level merge
  Doc := ParseTOML(
    'a.b.x = 1' + LineEnding +
    'a.b.y = 2' + LineEnding +
    'a.c = 3'
  );
  try
    AssertTrue('a exists', Doc.TryGetValue('a', Value));
    AssertTrue('b exists', Value.AsTable.TryGetValue('b', Value));
    AssertTrue('x exists', Value.AsTable.TryGetValue('x', Value));
    AssertEquals('x value', 1, Value.AsInteger);
    // Re-navigate for y
    Doc.TryGetValue('a', Value);
    Value.AsTable.TryGetValue('b', Value);
    AssertTrue('y exists', Value.AsTable.TryGetValue('y', Value));
    AssertEquals('y value', 2, Value.AsInteger);
    // Check c
    Doc.TryGetValue('a', Value);
    AssertTrue('c exists', Value.AsTable.TryGetValue('c', Value));
    AssertEquals('c value', 3, Value.AsInteger);
  finally
    Doc.Free;
  end;
end;
```

- [ ] **Step 4: Implement Test75 — duplicate dotted key error**

```pascal
procedure TTOMLTestCase.Test75_DuplicateDottedKeyError;
var
  Doc: TTOMLTable;
begin
  try
    Doc := ParseTOML(
      'physical.color = "red"' + LineEnding +
      'physical.color = "blue"'
    );
    Doc.Free;
    Fail('Should have raised duplicate key error');
  except
    on E: ETOMLParserException do
      ; // Expected
  end;
end;
```

- [ ] **Step 5: Implement Test76 — dotted keys in inline tables**

```pascal
procedure TTOMLTestCase.Test76_DottedKeysInInlineTables;
var
  Doc: TTOMLTable;
  Value, PointValue, XValue: TTOMLValue;
begin
  Doc := ParseTOML('point = { x.y = 1, x.z = 2 }');
  try
    AssertTrue('point exists', Doc.TryGetValue('point', PointValue));
    AssertTrue('x exists', PointValue.AsTable.TryGetValue('x', XValue));
    AssertTrue('y exists', XValue.AsTable.TryGetValue('y', Value));
    AssertEquals('y value', 1, Value.AsInteger);
    AssertTrue('z exists', XValue.AsTable.TryGetValue('z', Value));
    AssertEquals('z value', 2, Value.AsInteger);
  finally
    Doc.Free;
  end;
end;
```

- [ ] **Step 6: Commit**

```bash
git add tests/TestCaseTOML.pas
git commit -m "Add parser tests for dotted key nesting, merging, and error handling"
```

---

### Task 6: New Serializer Tests

**Files:**
- Modify: `tests/TestCaseTOML.pas:11-85` (add declarations)
- Modify: `tests/TestCaseTOML.pas` (append implementations)

- [ ] **Step 1: Add test declarations**

Add after the Task 5 declarations:
```pascal
    procedure Test77_SerializeNestedArrayOfTables;
    procedure Test78_SerializeSubTablesUnderArrayElements;
    procedure Test79_SerializeDeepNesting;
```

- [ ] **Step 2: Implement Test77 — nested array-of-tables serialization**

```pascal
procedure TTOMLTestCase.Test77_SerializeNestedArrayOfTables;
var
  Doc: TTOMLTable;
  Fruits, Varieties: TTOMLArray;
  FruitTable, VarietyTable: TTOMLTable;
  TOML: string;
  ParsedDoc: TTOMLTable;
  Value: TTOMLValue;
begin
  Doc := TTOMLTable.Create;
  try
    Fruits := TTOMLArray.Create;

    // First fruit with varieties
    FruitTable := TTOMLTable.Create;
    FruitTable.Add('name', TTOMLString.Create('apple'));
    Varieties := TTOMLArray.Create;
    VarietyTable := TTOMLTable.Create;
    VarietyTable.Add('name', TTOMLString.Create('red delicious'));
    Varieties.Add(VarietyTable);
    VarietyTable := TTOMLTable.Create;
    VarietyTable.Add('name', TTOMLString.Create('granny smith'));
    Varieties.Add(VarietyTable);
    FruitTable.Add('varieties', Varieties);
    Fruits.Add(FruitTable);

    // Second fruit with one variety
    FruitTable := TTOMLTable.Create;
    FruitTable.Add('name', TTOMLString.Create('banana'));
    Varieties := TTOMLArray.Create;
    VarietyTable := TTOMLTable.Create;
    VarietyTable.Add('name', TTOMLString.Create('plantain'));
    Varieties.Add(VarietyTable);
    FruitTable.Add('varieties', Varieties);
    Fruits.Add(FruitTable);

    Doc.Add('fruits', Fruits);

    TOML := SerializeTOML(Doc);

    // Verify correct headers
    AssertTrue('Has [[fruits]]', Pos('[[fruits]]', TOML) > 0);
    AssertTrue('Has [[fruits.varieties]]', Pos('[[fruits.varieties]]', TOML) > 0);

    // Round-trip verify
    ParsedDoc := ParseTOML(TOML);
    try
      AssertTrue('fruits exists', ParsedDoc.TryGetValue('fruits', Value));
      AssertEquals('fruits count', 2, Value.AsArray.Count);
      // First fruit varieties
      Value.AsArray.GetItem(0).AsTable.TryGetValue('varieties', Value);
      AssertEquals('first fruit varieties count', 2, Value.AsArray.Count);
    finally
      ParsedDoc.Free;
    end;
  finally
    Doc.Free;
  end;
end;
```

- [ ] **Step 3: Implement Test78 — sub-tables under array elements**

```pascal
procedure TTOMLTestCase.Test78_SerializeSubTablesUnderArrayElements;
var
  Doc: TTOMLTable;
  Fruits: TTOMLArray;
  FruitTable, PhysicalTable: TTOMLTable;
  TOML: string;
  ParsedDoc: TTOMLTable;
  Value: TTOMLValue;
begin
  Doc := TTOMLTable.Create;
  try
    Fruits := TTOMLArray.Create;

    FruitTable := TTOMLTable.Create;
    FruitTable.Add('name', TTOMLString.Create('apple'));
    PhysicalTable := TTOMLTable.Create;
    PhysicalTable.Add('color', TTOMLString.Create('red'));
    PhysicalTable.Add('shape', TTOMLString.Create('round'));
    FruitTable.Add('physical', PhysicalTable);
    Fruits.Add(FruitTable);

    Doc.Add('fruits', Fruits);

    TOML := SerializeTOML(Doc);

    AssertTrue('Has [[fruits]]', Pos('[[fruits]]', TOML) > 0);
    AssertTrue('Has [fruits.physical]', Pos('[fruits.physical]', TOML) > 0);
    // Make sure it's not [[fruits.physical]]
    AssertEquals('No double-bracket for sub-table', 0, Pos('[[fruits.physical]]', TOML));

    // Round-trip
    ParsedDoc := ParseTOML(TOML);
    try
      ParsedDoc.TryGetValue('fruits', Value);
      Value.AsArray.GetItem(0).AsTable.TryGetValue('physical', Value);
      AssertEquals('color', 'red',
        Value.AsTable.Items['color'].AsString);
    finally
      ParsedDoc.Free;
    end;
  finally
    Doc.Free;
  end;
end;
```

- [ ] **Step 4: Implement Test79 — deep nesting**

```pascal
procedure TTOMLTestCase.Test79_SerializeDeepNesting;
var
  Doc, CTable: TTOMLTable;
  AArray, BArray, CArray: TTOMLArray;
  ATable, BTable: TTOMLTable;
  TOML: string;
  ParsedDoc: TTOMLTable;
  Value: TTOMLValue;
begin
  // Build: a -> [Table{val=1, b -> [Table{val=10, c -> [Table{val=100}]}]}]
  Doc := TTOMLTable.Create;
  try
    AArray := TTOMLArray.Create;
    ATable := TTOMLTable.Create;
    ATable.Add('val', TTOMLInteger.Create(1));

    BArray := TTOMLArray.Create;
    BTable := TTOMLTable.Create;
    BTable.Add('val', TTOMLInteger.Create(10));

    CArray := TTOMLArray.Create;
    CTable := TTOMLTable.Create;
    CTable.Add('val', TTOMLInteger.Create(100));
    CArray.Add(CTable);

    BTable.Add('c', CArray);
    BArray.Add(BTable);
    ATable.Add('b', BArray);
    AArray.Add(ATable);
    Doc.Add('a', AArray);

    TOML := SerializeTOML(Doc);

    AssertTrue('Has [[a]]', Pos('[[a]]', TOML) > 0);
    AssertTrue('Has [[a.b]]', Pos('[[a.b]]', TOML) > 0);
    AssertTrue('Has [[a.b.c]]', Pos('[[a.b.c]]', TOML) > 0);

    // Round-trip
    ParsedDoc := ParseTOML(TOML);
    try
      ParsedDoc.TryGetValue('a', Value);
      AssertEquals('a count', 1, Value.AsArray.Count);
      Value.AsArray.GetItem(0).AsTable.TryGetValue('b', Value);
      AssertEquals('b count', 1, Value.AsArray.Count);
      Value.AsArray.GetItem(0).AsTable.TryGetValue('c', Value);
      AssertEquals('c count', 1, Value.AsArray.Count);
      AssertEquals('c val', 100,
        Value.AsArray.GetItem(0).AsTable.Items['val'].AsInteger);
    finally
      ParsedDoc.Free;
    end;
  finally
    Doc.Free;
  end;
end;
```

- [ ] **Step 5: Commit**

```bash
git add tests/TestCaseTOML.pas
git commit -m "Add serializer tests for nested arrays-of-tables, sub-tables, and deep nesting"
```

---

### Task 7: Round-Trip Tests

**Files:**
- Modify: `tests/TestCaseTOML.pas:11-85` (add declarations)
- Modify: `tests/TestCaseTOML.pas` (append implementations)

- [ ] **Step 1: Add test declarations**

Add after the Task 6 declarations:
```pascal
    procedure Test80_RoundTripCanonicalFruitsExample;
    procedure Test81_RoundTripDeepNestedArrayChains;
```

- [ ] **Step 2: Implement Test80 — TOML spec canonical example round-trip**

```pascal
procedure TTOMLTestCase.Test80_RoundTripCanonicalFruitsExample;
var
  TOML, Serialized: string;
  Doc, ParsedDoc: TTOMLTable;
  Value, SubValue: TTOMLValue;
  Fruits: TTOMLArray;
begin
  // The canonical example from the TOML v1.0.0 spec
  TOML := '[[fruits]]' + LineEnding +
          'name = "apple"' + LineEnding +
          '' + LineEnding +
          '[fruits.physical]' + LineEnding +
          'color = "red"' + LineEnding +
          'shape = "round"' + LineEnding +
          '' + LineEnding +
          '[[fruits.varieties]]' + LineEnding +
          'name = "red delicious"' + LineEnding +
          '' + LineEnding +
          '[[fruits.varieties]]' + LineEnding +
          'name = "granny smith"' + LineEnding +
          '' + LineEnding +
          '[[fruits]]' + LineEnding +
          'name = "banana"' + LineEnding +
          '' + LineEnding +
          '[[fruits.varieties]]' + LineEnding +
          'name = "plantain"';

  // Parse
  Doc := ParseTOML(TOML);
  try
    // Verify structure
    AssertTrue('fruits exists', Doc.TryGetValue('fruits', Value));
    Fruits := Value.AsArray;
    AssertEquals('fruits count', 2, Fruits.Count);

    // First fruit
    AssertTrue('apple name', Fruits.GetItem(0).AsTable.TryGetValue('name', Value));
    AssertEquals('apple', Value.AsString);
    AssertTrue('physical', Fruits.GetItem(0).AsTable.TryGetValue('physical', Value));
    AssertTrue('color', Value.AsTable.TryGetValue('color', SubValue));
    AssertEquals('red', SubValue.AsString);
    AssertTrue('varieties', Fruits.GetItem(0).AsTable.TryGetValue('varieties', Value));
    AssertEquals('apple varieties', 2, Value.AsArray.Count);

    // Second fruit
    AssertTrue('banana name', Fruits.GetItem(1).AsTable.TryGetValue('name', Value));
    AssertEquals('banana', Value.AsString);
    AssertTrue('banana varieties', Fruits.GetItem(1).AsTable.TryGetValue('varieties', Value));
    AssertEquals('banana variety count', 1, Value.AsArray.Count);

    // Serialize
    Serialized := SerializeTOML(Doc);

    // Verify serialized output contains correct headers
    AssertTrue('Serialized has [[fruits]]', Pos('[[fruits]]', Serialized) > 0);
    AssertTrue('Serialized has [fruits.physical]', Pos('[fruits.physical]', Serialized) > 0);
    AssertTrue('Serialized has [[fruits.varieties]]', Pos('[[fruits.varieties]]', Serialized) > 0);

    // Round-trip: parse the serialized output
    ParsedDoc := ParseTOML(Serialized);
    try
      AssertTrue('RT fruits', ParsedDoc.TryGetValue('fruits', Value));
      AssertEquals('RT fruits count', 2, Value.AsArray.Count);
      // Verify first fruit still has physical and varieties
      AssertTrue('RT physical',
        Value.AsArray.GetItem(0).AsTable.TryGetValue('physical', SubValue));
      AssertTrue('RT varieties',
        Value.AsArray.GetItem(0).AsTable.TryGetValue('varieties', SubValue));
      AssertEquals('RT varieties count', 2, SubValue.AsArray.Count);
    finally
      ParsedDoc.Free;
    end;
  finally
    Doc.Free;
  end;
end;
```

- [ ] **Step 3: Implement Test81 — deep nested array chain round-trip**

```pascal
procedure TTOMLTestCase.Test81_RoundTripDeepNestedArrayChains;
var
  TOML, Serialized: string;
  Doc, ParsedDoc: TTOMLTable;
  Value: TTOMLValue;
begin
  TOML := '[[a]]' + LineEnding +
          'val = 1' + LineEnding +
          '' + LineEnding +
          '[[a.b]]' + LineEnding +
          'val = 10' + LineEnding +
          '' + LineEnding +
          '[[a.b.c]]' + LineEnding +
          'val = 100' + LineEnding +
          '' + LineEnding +
          '[[a.b.c]]' + LineEnding +
          'val = 101' + LineEnding +
          '' + LineEnding +
          '[[a]]' + LineEnding +
          'val = 2' + LineEnding +
          '' + LineEnding +
          '[[a.b]]' + LineEnding +
          'val = 30';

  Doc := ParseTOML(TOML);
  try
    // Verify parsed structure
    AssertTrue('a exists', Doc.TryGetValue('a', Value));
    AssertEquals('a count', 2, Value.AsArray.Count);

    // a[0].b[0].c has 2 entries
    Value.AsArray.GetItem(0).AsTable.TryGetValue('b', Value);
    AssertEquals('a[0].b count', 1, Value.AsArray.Count);
    Value.AsArray.GetItem(0).AsTable.TryGetValue('c', Value);
    AssertEquals('a[0].b[0].c count', 2, Value.AsArray.Count);

    // Serialize and round-trip
    Serialized := SerializeTOML(Doc);

    AssertTrue('Has [[a]]', Pos('[[a]]', Serialized) > 0);
    AssertTrue('Has [[a.b]]', Pos('[[a.b]]', Serialized) > 0);
    AssertTrue('Has [[a.b.c]]', Pos('[[a.b.c]]', Serialized) > 0);

    ParsedDoc := ParseTOML(Serialized);
    try
      AssertTrue('RT a exists', ParsedDoc.TryGetValue('a', Value));
      AssertEquals('RT a count', 2, Value.AsArray.Count);
      // Verify deep nesting preserved
      Value.AsArray.GetItem(0).AsTable.TryGetValue('b', Value);
      Value.AsArray.GetItem(0).AsTable.TryGetValue('c', Value);
      AssertEquals('RT c count', 2, Value.AsArray.Count);
    finally
      ParsedDoc.Free;
    end;
  finally
    Doc.Free;
  end;
end;
```

- [ ] **Step 4: Commit**

```bash
git add tests/TestCaseTOML.pas
git commit -m "Add round-trip tests for canonical fruits example and deep nested array chains"
```

---

### Task 8: Final Verification and Cleanup

**Files:**
- All modified files

- [ ] **Step 1: Verify Delphi compilation of all source units**

Run: `mcp__delphi-build__compile_delphi_project` with `force_build_all: true` on an example project that uses all units.
Expected: No compilation errors.

- [ ] **Step 2: Review all existing tests still logically correct**

Read through the test file and check that no other tests depend on the old flat dotted-key behavior. Key tests to verify:
- `Test60_DottedTableArray` (line 907): Uses inline tables for physical/varieties — no dotted keys in key-value pairs, should be unaffected.
- `Test69_TableArrayNesting` (line 1255): Same pattern, unaffected.
- `Test70_ComplexKeys` (line 1298): Uses `"quoted.key"` at top level — stored as `quoted.key` in root table. This is a quoted key (not a dotted key), so behavior is unchanged.
- `Test72_LiteralDottedKeyTable` (line 1404): Uses literal dotted key `tatter.man` as a quoted key inside a table — unaffected.

- [ ] **Step 3: Commit final state if any adjustments needed**

```bash
git add -A
git commit -m "Final adjustments for TOML v1.0.0 nested key compliance"
```

- [ ] **Step 4: Run full test suite with FPC (if available)**

Run: `lazbuild tests/TestRunner.lpi && ./tests/TestRunner --all -f`
Expected: All tests pass (existing + 11 new).

If FPC is not available on this system, note this as a manual verification step.
