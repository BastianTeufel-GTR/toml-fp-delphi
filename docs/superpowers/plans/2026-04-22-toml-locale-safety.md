# TOML Locale Safety Net Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the toml-fp serializer produce valid TOML regardless of the host locale's `DecimalSeparator`, by routing all locale-sensitive formatting through a shared invariant `TFormatSettings`.

**Architecture:** Add a library-wide `TOMLFormatSettings: TFormatSettings` in the `TOML.Types` unit (initialized once in that unit's `initialization` section with `DecimalSeparator = '.'` and `ThousandSeparator = #0`). Route the serializer's `FloatToStr` and `FormatDateTime` calls through it. Parser stays as-is — `Val()` is already locale-independent. Validate with a locale-sabotage test that temporarily sets `DecimalSeparator := ','`.

**Tech Stack:** Delphi 12 / FPC (dual-target), DUnitX (Delphi tests), FPCUnit (FPC tests), delphi-build MCP server for compilation.

**Spec:** `docs/superpowers/specs/2026-04-22-toml-locale-safety-design.md`

---

## File Structure

| Path | Change | Responsibility |
|------|--------|----------------|
| `src/TOML.Types.pas` | Modify | Declare and initialize the shared `TOMLFormatSettings`. |
| `src/TOML.Serializer.pas` | Modify | Use `TOMLFormatSettings` for `FloatToStr` and `FormatDateTime`. |
| `tests/delphi/TOML.Tests.Locale.pas` | Create | DUnitX locale-sabotage test (Delphi). |
| `tests/delphi/TOML.Tests.dpr` | Modify | Register the new DUnitX unit in the test runner. |
| `tests/TestCaseTOML.pas` | Modify | Mirror the locale-sabotage test for FPCUnit (FPC target). |

---

## Task 1: Add `TOMLFormatSettings` to `TOML.Types`

**Files:**
- Modify: `src/TOML.Types.pas` (interface section near top, implementation end before `end.`)

- [ ] **Step 1: Add the variable declaration to the interface section**

In `src/TOML.Types.pas`, locate the `interface` `uses` block (currently line 25-26):
```pascal
uses
  SysUtils, Classes, Generics.Collections;
```

Immediately **before** the `type` keyword on line 28, insert:

```pascal
var
  /// <summary>
  /// Locale-invariant format settings used throughout toml-fp for numeric
  /// and date formatting. Ensures the serializer emits '.' as the decimal
  /// separator regardless of the host locale. Advanced consumers that
  /// hand-build TOML fragments can reuse this to stay spec-compliant.
  /// </summary>
  TOMLFormatSettings: TFormatSettings;

```

- [ ] **Step 2: Add the `initialization` section**

Open `src/TOML.Types.pas` and locate the final `end.` (line 565). Replace the final lines:

```pascal
function TTOMLTable.GetAsTable: TTOMLTable;
begin
  Result := Self;
end;

end.
```

with:

```pascal
function TTOMLTable.GetAsTable: TTOMLTable;
begin
  Result := Self;
end;

initialization
  {$IFDEF FPC}
  TOMLFormatSettings := DefaultFormatSettings;
  {$ELSE}
  TOMLFormatSettings := FormatSettings;
  {$ENDIF}
  TOMLFormatSettings.DecimalSeparator := '.';
  TOMLFormatSettings.ThousandSeparator := #0;

end.
```

- [ ] **Step 3: Compile with delphi-build MCP (Win64)**

Use the `mcp__delphi-build__compile_delphi_project` tool to compile `tests/delphi/TOML.Tests.dproj` for `Win64`.

Expected: compilation succeeds (no changes to public API, pure addition).

- [ ] **Step 4: Commit**

```bash
git add src/TOML.Types.pas
git commit -m "Add TOMLFormatSettings invariant for locale-safe formatting"
```

---

## Task 2: Write failing DUnitX locale-sabotage test

**Files:**
- Create: `tests/delphi/TOML.Tests.Locale.pas`
- Modify: `tests/delphi/TOML.Tests.dpr`

- [ ] **Step 1: Create the test unit**

Create `tests/delphi/TOML.Tests.Locale.pas` with this exact content:

```pascal
{*******************************************************************************
  Unit Name: TOML.Tests.Locale
  Purpose: DUnitX test verifying that serialization is independent of the
           host locale's DecimalSeparator.

  Copyright (c) 1984-2026 GTR mbH

  Dependencies:
    - DUnitX.TestFramework
    - TOML, TOML.Types, TOML.Serializer
*******************************************************************************}
unit TOML.Tests.Locale;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  TOML,
  TOML.Types;

type
  /// <summary>
  /// Verifies that <c>TTOMLSerializer</c> emits '.' as the float decimal
  /// separator even when the host locale's <c>DecimalSeparator</c> is ','.
  /// Guards against regressions that would re-introduce a global-locale
  /// dependency into float or datetime serialization.
  /// </summary>
  [TestFixture]
  TTOMLLocaleTests = class
  private
    FSavedDecimalSeparator: Char;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  published
    /// <summary>
    /// With <c>DecimalSeparator := ','</c>, serialized output must contain
    /// "3.14" and "2.718" (dot) and must NOT contain "3,14" or "2,718"
    /// (comma), and must round-trip back through the parser with floats
    /// preserved within 1e-9.
    /// </summary>
    [Test]
    procedure Serialize_LocaleWithCommaDecimalSeparator_EmitsDotNotComma;
  end;

implementation

uses
  System.Math,
  TOML.Serializer;

procedure TTOMLLocaleTests.Setup;
begin
  FSavedDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := ',';
end;

procedure TTOMLLocaleTests.TearDown;
begin
  FormatSettings.DecimalSeparator := FSavedDecimalSeparator;
end;

procedure TTOMLLocaleTests.Serialize_LocaleWithCommaDecimalSeparator_EmitsDotNotComma;
var
  LTable: TTOMLTable;
  LSerialized: string;
  LReparsed: TTOMLTable;
  LValue: TTOMLValue;
begin
  LTable := TOMLTable;
  try
    LTable.Add('pi', TOMLFloat(3.14));
    LTable.Add('e', TOMLFloat(2.718));

    LSerialized := SerializeTOML(LTable);

    Assert.Contains(LSerialized, '3.14',
      'Serialized output must contain "3.14" (dot separator)');
    Assert.Contains(LSerialized, '2.718',
      'Serialized output must contain "2.718" (dot separator)');
    Assert.IsFalse(LSerialized.Contains('3,14'),
      'Serialized output must NOT contain "3,14" (locale comma leaked)');
    Assert.IsFalse(LSerialized.Contains('2,718'),
      'Serialized output must NOT contain "2,718" (locale comma leaked)');

    LReparsed := ParseTOML(LSerialized);
    try
      Assert.IsTrue(LReparsed.TryGetValue('pi', LValue), 'pi key present after round-trip');
      Assert.IsTrue(SameValue(LValue.AsFloat, 3.14, 1e-9),
        'pi round-trips within 1e-9');
      Assert.IsTrue(LReparsed.TryGetValue('e', LValue), 'e key present after round-trip');
      Assert.IsTrue(SameValue(LValue.AsFloat, 2.718, 1e-9),
        'e round-trips within 1e-9');
    finally
      LReparsed.Free;
    end;
  finally
    LTable.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTOMLLocaleTests);

end.
```

- [ ] **Step 2: Register the new unit in `TOML.Tests.dpr`**

Open `tests/delphi/TOML.Tests.dpr`, locate the `uses` entries (around lines 20-22):

```pascal
  TOML.Tests.NestedKeys in 'TOML.Tests.NestedKeys.pas',
  TOML.Tests.Comments in 'TOML.Tests.Comments.pas',
  TOML.Tests.MultilineStrings in 'TOML.Tests.MultilineStrings.pas';
```

Change the final semicolon to a comma and add the new line so it reads:

```pascal
  TOML.Tests.NestedKeys in 'TOML.Tests.NestedKeys.pas',
  TOML.Tests.Comments in 'TOML.Tests.Comments.pas',
  TOML.Tests.MultilineStrings in 'TOML.Tests.MultilineStrings.pas',
  TOML.Tests.Locale in 'TOML.Tests.Locale.pas';
```

- [ ] **Step 3: Compile and run the tests — expect the new test to FAIL**

Compile `tests/delphi/TOML.Tests.dproj` for `Win64` via the `mcp__delphi-build__compile_delphi_project` tool.

Then run the resulting test executable (e.g. `tests/delphi/Win64/Debug/TOML.Tests.exe`).

Expected: the **new test `Serialize_LocaleWithCommaDecimalSeparator_EmitsDotNotComma` FAILS** because the serializer still emits `3,14` under the sabotaged locale. All other tests continue to pass.

If the test unexpectedly passes, something is wrong — either the locale override didn't take effect (check that `FormatSettings.DecimalSeparator` is actually `,` inside Setup) or the serializer was already fixed. Do **not** proceed until you see the expected failure mode.

- [ ] **Step 4: Commit**

```bash
git add tests/delphi/TOML.Tests.Locale.pas tests/delphi/TOML.Tests.dpr
git commit -m "Add failing DUnitX test for locale-independent float serialization"
```

---

## Task 3: Fix the serializer to use `TOMLFormatSettings`

**Files:**
- Modify: `src/TOML.Serializer.pas:448` (float) and `src/TOML.Serializer.pas:388` (datetime)

Context: `TOML.Serializer` already imports `TOML.Types` in its interface `uses` clause (line 33), so `TOMLFormatSettings` is visible without further changes.

- [ ] **Step 1: Change the float `FloatToStr` call**

In `src/TOML.Serializer.pas` around line 448, change:

```pascal
    tvtFloat:
      FStringBuilder.Append(FloatToStr(AValue.AsFloat));
```

to:

```pascal
    tvtFloat:
      FStringBuilder.Append(FloatToStr(AValue.AsFloat, TOMLFormatSettings));
```

- [ ] **Step 2: Change the datetime `FormatDateTime` call**

In `src/TOML.Serializer.pas` around line 388, change:

```pascal
procedure TTOMLSerializer.WriteDateTime(const ADateTime: TDateTime);
begin
  FStringBuilder.Append(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', ADateTime));
```

to:

```pascal
procedure TTOMLSerializer.WriteDateTime(const ADateTime: TDateTime);
begin
  FStringBuilder.Append(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', ADateTime, TOMLFormatSettings));
```

(Line numbers are approximate — they may shift if earlier edits changed file length. Match by the exact code snippet, not the line number.)

- [ ] **Step 3: Compile and run the tests — expect all tests to PASS**

Compile `tests/delphi/TOML.Tests.dproj` for `Win64` via `mcp__delphi-build__compile_delphi_project`.

Run the test executable.

Expected: **all tests pass**, including `Serialize_LocaleWithCommaDecimalSeparator_EmitsDotNotComma`.

- [ ] **Step 4: Commit**

```bash
git add src/TOML.Serializer.pas
git commit -m "Route serializer float and datetime formatting through TOMLFormatSettings"
```

---

## Task 4: Mirror the locale-sabotage test in FPCUnit

**Files:**
- Modify: `tests/TestCaseTOML.pas`

Context: `TestCaseTOML.pas` uses FPCUnit (not DUnitX). Published test methods are declared in a class, registered automatically via `testregistry`. The highest existing test number is in the `Test80`+ range.

- [ ] **Step 1: Declare the test method in the class interface**

In `tests/TestCaseTOML.pas`, find the last `procedure Test..` declaration in the `published` section of `TTOMLTestCase`. Append a new line immediately after the last existing procedure declaration (before the closing `end;` of the class):

```pascal
    { Locale safety }
    procedure Test90_SerializeLocaleWithCommaDecimalSeparator;
```

- [ ] **Step 2: Implement the test method**

In the implementation section of the same file, append this procedure after the last existing test implementation and before the final `end.`:

```pascal
procedure TTOMLTestCase.Test90_SerializeLocaleWithCommaDecimalSeparator;
var
  SavedDecimalSeparator: Char;
  Table: TTOMLTable;
  Reparsed: TTOMLTable;
  Serialized: string;
  Value: TTOMLValue;
begin
  SavedDecimalSeparator := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := ',';
  try
    Table := TOMLTable;
    try
      Table.Add('pi', TOMLFloat(3.14));
      Table.Add('e', TOMLFloat(2.718));

      Serialized := SerializeTOML(Table);

      AssertTrue('Serialized output must contain "3.14"',
        Pos('3.14', Serialized) > 0);
      AssertTrue('Serialized output must contain "2.718"',
        Pos('2.718', Serialized) > 0);
      AssertTrue('Serialized output must NOT contain "3,14"',
        Pos('3,14', Serialized) = 0);
      AssertTrue('Serialized output must NOT contain "2,718"',
        Pos('2,718', Serialized) = 0);

      Reparsed := ParseTOML(Serialized);
      try
        AssertTrue('pi key present after round-trip',
          Reparsed.TryGetValue('pi', Value));
        AssertTrue('pi round-trips within 1e-9',
          SameValue(Value.AsFloat, 3.14, 1e-9));
        AssertTrue('e key present after round-trip',
          Reparsed.TryGetValue('e', Value));
        AssertTrue('e round-trips within 1e-9',
          SameValue(Value.AsFloat, 2.718, 1e-9));
      finally
        Reparsed.Free;
      end;
    finally
      Table.Free;
    end;
  finally
    DefaultFormatSettings.DecimalSeparator := SavedDecimalSeparator;
  end;
end;
```

Note: the file already uses `Math` in its `uses` (for `SameValue`) — no new imports needed.

- [ ] **Step 3: Build and run the FPC test runner**

From the repo root, build `tests/TestRunner.lpi` using Lazarus / `lazbuild`:

```bash
lazbuild tests/TestRunner.lpi
```

Expected: build succeeds.

Then run the resulting executable (e.g. `tests/TestRunner`) with `--all`:

```bash
tests/TestRunner --all
```

Expected: **all tests pass**, including `Test90_SerializeLocaleWithCommaDecimalSeparator`.

If `lazbuild` is not available in the environment, skip the build step but document that this test must be run manually in Lazarus before merging.

- [ ] **Step 4: Commit**

```bash
git add tests/TestCaseTOML.pas
git commit -m "Mirror locale-independent float serialization test in FPCUnit suite"
```

---

## Task 5: Final verification

- [ ] **Step 1: Re-run the full Delphi test suite**

Compile and run `tests/delphi/TOML.Tests.dproj` one more time on `Win64`. Confirm every fixture green.

- [ ] **Step 2: Review the diff end-to-end**

```bash
git log --oneline main..HEAD
git diff main..HEAD
```

Confirm the diff matches the four logical commits (Types var, failing test, serializer fix, FPCUnit mirror) with no stray changes.

- [ ] **Step 3: Update `CHANGELOG.md`**

Open `CHANGELOG.md` and add a new entry at the top describing the fix. Use the existing file's format (check the most recent entry for reference). The entry should mention:

- Fixed: float serialization now emits `.` as the decimal separator regardless of host locale.
- Added: public `TOMLFormatSettings: TFormatSettings` for consumers that hand-build TOML fragments.

Commit:

```bash
git add CHANGELOG.md
git commit -m "Update CHANGELOG for locale-safe serialization"
```

---

## Self-Review Notes

- **Spec coverage:** §2.1 → Task 1; §2.2 → Task 3; §2.3 → untouched as specified; §3 → Tasks 2 & 4; §5 acceptance criteria → Tasks 1-4 with cross-platform verification.
- **Red flag scan:** no TBDs, every code block is complete, expected test output stated explicitly.
- **Type consistency:** `TOMLFormatSettings` name is stable across Tasks 1, 3; test names consistent with spec (`Serialize_LocaleWithCommaDecimalSeparator_EmitsDotNotComma` / `Test90_SerializeLocaleWithCommaDecimalSeparator`).
- **Small deviation from spec:** spec said "DUnitX test in `tests/TestCaseTOML.pas`" but that file is FPCUnit. Plan resolves by placing the DUnitX test in the correct DUnitX location (`tests/delphi/TOML.Tests.Locale.pas`) and adding a parallel FPCUnit test in `TestCaseTOML.pas` — honors the spec's "Works on both Delphi 12 and FPC" acceptance criterion.
