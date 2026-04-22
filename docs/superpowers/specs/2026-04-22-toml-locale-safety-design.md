# TOML Locale Safety Net — Design

**Date:** 2026-04-22
**Status:** Approved (brainstorming complete)
**Author:** Bastian Teufel

## 1. Problem

`TOML.Serializer.WriteValue` at `src/TOML.Serializer.pas:448` calls
`FloatToStr(AValue.AsFloat)` without an explicit `TFormatSettings`. On a
machine whose locale sets `DecimalSeparator := ','` (German, French, and
most of continental Europe), this emits invalid TOML such as
`pi = 3,14`.

The parser uses `Val()` at `src/TOML.Parser.pas:1093`, which is
locale-independent in both Delphi and FPC, so *loading* TOML is already
safe. The bug is strictly on the serializer side — but it produces
output that cannot be round-tripped through the library's own loader.

Typical trigger: an application starts with its OS locale, writes a
config file via toml-fp before any `FormatSettings` override is
applied, and the resulting file is corrupt.

## 2. Solution

Introduce one library-wide invariant `TFormatSettings` and route all
locale-sensitive formatting through it.

### 2.1 `TOML.Types.pas` — shared invariant settings

Add to the unit `interface`:

```pascal
var
  TOMLFormatSettings: TFormatSettings;
```

Initialize it in the unit's `initialization` section:

```pascal
initialization
  TOMLFormatSettings := {$IFDEF FPC}DefaultFormatSettings{$ELSE}FormatSettings{$ENDIF};
  TOMLFormatSettings.DecimalSeparator := '.';
  TOMLFormatSettings.ThousandSeparator := #0;
```

One copy, initialized once at unit load, public so advanced users can
reuse it (e.g. when hand-building TOML fragments).

### 2.2 `TOML.Serializer.pas` — use the invariant settings

- `FloatToStr(AValue.AsFloat)` → `FloatToStr(AValue.AsFloat, TOMLFormatSettings)`
- `FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', ADateTime)` →
  `FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', ADateTime, TOMLFormatSettings)`
  (defensive — the literal `.` before `zzz` is already safe, but
  passing the settings closes the whole category and documents intent).

### 2.3 `TOML.Parser.pas` — no functional change

`Val()` is locale-independent; leave as-is. The shared
`TOMLFormatSettings` exists for the serializer and library consumers,
not for `Val`.

## 3. Testing

One new DUnitX test in `tests/TestCaseTOML.pas`:

`TestSerialize_LocaleWithCommaDecimalSeparator_EmitsDotNotComma`

Procedure:

1. Save the current `DecimalSeparator`
   (`FormatSettings.DecimalSeparator` on Delphi,
   `DefaultFormatSettings.DecimalSeparator` on FPC).
2. In a `try`/`finally` block, set it to `','`.
3. Build a `TTOMLTable` containing two float entries, e.g.
   `pi = 3.14` and `e = 2.718`.
4. Serialize to a string via `TTOMLSerializer`.
5. Assert the output **contains** `'3.14'` and `'2.718'`.
6. Assert the output **does not contain** `'3,14'` or `'2,718'`.
7. Parse the output back and assert the float values round-trip within
   `1e-9`.
8. Restore `DecimalSeparator` in `finally`.

If a future refactor regresses the serializer to the global
`FormatSettings`, this test fails loudly.

## 4. Out of Scope

- Refactoring existing float tests to run under multiple locales
  (low-value churn; one dedicated test covers the guarantee).
- Changing the parser (already safe via `Val`).
- Exposing helper functions like `TOMLFloatToStr` — call sites using
  `FloatToStr(x, TOMLFormatSettings)` are short and explicit enough.

## 5. Acceptance Criteria

- `TOMLFormatSettings` is declared in `TOML.Types` interface and
  initialized with `DecimalSeparator = '.'` and
  `ThousandSeparator = #0`.
- The serializer uses `TOMLFormatSettings` for both `FloatToStr` and
  `FormatDateTime`.
- The new locale-sabotage test passes.
- All existing tests continue to pass.
- Works on both Delphi 12 and FPC (the two targets currently supported
  by the code's `{$IF defined(FPC)}` splits).
