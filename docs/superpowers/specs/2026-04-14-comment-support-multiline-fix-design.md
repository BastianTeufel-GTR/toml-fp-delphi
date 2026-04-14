# Design: TOML Comment Support and Multiline String Fix

**Date:** 2026-04-14
**Version:** toml-fp v1.2.0
**Scope:** Full round-trip comment preservation, multiline string fixes, backward compatibility

## Background

The toml-fp library is fully TOML v1.0.0 compliant for data types and structures. Two gaps remain:

1. Comments are discarded during parsing. Users need to read, write, and preserve comments in round-trip scenarios (e.g., configuration files with documentation comments).
2. Multiline strings (`"""` and `'''`) are parsed correctly but the serializer always flattens them to basic double-quoted strings. The line-ending backslash feature in `"""` strings is not implemented.

Reference implementations analyzed: Python tomlkit (Trivia-based approach) and Rust toml-rs/toml_edit (Decor-based approach).

## Design Decisions

- **Ordered body list with dictionary index** for `TTOMLTable` (tomlkit pattern). The body list preserves insertion order and stores comments as entries. The dictionary provides fast key lookups.
- **Normalized whitespace** during serialization. No byte-perfect preservation of original indentation or spacing.
- **Comment scope limited to:** standalone full-line comments and inline comments after values. Table header comments, end-of-file comments, and comments inside multiline arrays are out of scope.
- **String style metadata** on `TTOMLString` to preserve the original quoting style through round-trips.

## Work Stream 1: Comment Support

### 1.1 New Types (TOML.Types.pas)

Add `tvtComment` to `TTOMLValueType`.

New class `TTOMLComment`:

```pascal
TTOMLComment = class(TTOMLValue)
private
  FText: string;
public
  constructor Create(const AText: string);
  property Text: string read FText write FText;
end;
```

- Stores comment text without the `# ` prefix.
- The `# ` prefix is added by the serializer during output.

### 1.2 Inline Comment on TTOMLValue (TOML.Types.pas)

Add to the base class `TTOMLValue`:

```pascal
TTOMLValue = class
private
  FValueType: TTOMLValueType;
  FInlineComment: string;
public
  property InlineComment: string read FInlineComment write FInlineComment;
end;
```

- Empty string means no inline comment.
- Stores comment text without the `# ` prefix.

### 1.3 TTOMLTable Restructure (TOML.Types.pas)

New record type:

```pascal
TTOMLTableEntry = record
  Key: string;       // Empty string for standalone comments
  Value: TTOMLValue;  // TTOMLComment for comments, other types for values
end;
```

New generic list type:

```pascal
TTOMLTableEntryList = TList<TTOMLTableEntry>;
```

Restructured `TTOMLTable`:

```pascal
TTOMLTable = class(TTOMLValue)
private
  FBody: TTOMLTableEntryList;  // Ordered entries (comments + key-value pairs)
  FIndex: TTOMLTableDict;       // Fast lookup by key (excludes comments)
public
  procedure Add(const AKey: string; AValue: TTOMLValue);
  procedure AddComment(const AText: string);
  function TryGetValue(const AKey: string; out AValue: TTOMLValue): Boolean;
  property Items: TTOMLTableDict read FIndex;  // Backward compatibility
  property Body: TTOMLTableEntryList read FBody;
end;
```

- `Add(Key, Value)` appends to `FBody` and inserts into `FIndex`. Existing merge logic for duplicate table keys is preserved.
- `AddComment(Text)` creates a `TTOMLComment` and appends a body entry with an empty key.
- `TryGetValue` reads from `FIndex` (unchanged behavior).
- `Destroy` iterates `FBody` and frees all `Value` fields (this includes both `TTOMLComment` entries and regular values). Then frees `FBody` and `FIndex`. `FIndex` must NOT free its values since they are the same object references already freed via `FBody`. Use `FIndex.Free` (not `FreeAndNil` with value cleanup) — the dictionary owns no values, only key->pointer mappings.

### 1.4 Parser Changes (TOML.Parser.pas)

**Comment capture:** When the parser encounters a `#` character at the start of a line (after optional whitespace), instead of skipping the line, it extracts the comment text and calls `CurrentTable.AddComment(Text)`.

**Inline comment capture:** After parsing a key-value pair and before consuming the newline, if a `#` character is found on the remainder of the line, the parser extracts the text and sets `Value.InlineComment := Text`.

### 1.5 Serializer Changes (TOML.Serializer.pas)

**Body-order iteration:** The serializer iterates `TTOMLTable.Body` instead of `TTOMLTable.Items`. For each entry:
- If the value is `TTOMLComment`, emit `# <text>` followed by a newline.
- Otherwise, emit the key-value pair.

**Inline comment output:** After serializing a value (before the newline), if `Value.InlineComment` is non-empty, append ` # <comment text>`.

### 1.6 Programmatic API

```pascal
// Standalone comments (before a key)
Table.AddComment('default STL resolution');
Table.AddComment('this value is used for slicing a STL chipbreaker file');
Table.AddComment('dimension in mm, smaller value = higher accuracy');
Table.Add('stl_resolution', TOMLFloat(0.5));

// Inline comment (after a value on the same line)
Value.InlineComment := 'default value';
```

Multiple `AddComment` calls produce multiple comment lines in the order they were added, appearing before the next `Add` call in the serialized output.

## Work Stream 2: Multiline String Fix

### 2.1 String Style Metadata (TOML.Types.pas)

New enum:

```pascal
TTOMLStringStyle = (tssBasic, tssLiteral, tssMultilineBasic, tssMultilineLiteral);
```

Enhanced `TTOMLString`:

```pascal
TTOMLString = class(TTOMLValue)
private
  FValue: string;
  FStyle: TTOMLStringStyle;
public
  constructor Create(const AValue: string; AStyle: TTOMLStringStyle = tssBasic);
  property Style: TTOMLStringStyle read FStyle write FStyle;
end;
```

Default is `tssBasic` so existing code creating `TTOMLString.Create('value')` continues to work.

### 2.2 Parser: Line-Ending Backslash (TOML.Parser.pas)

In the `"""` multiline string parser, when a `\` is followed by a newline (LF or CRLF):
1. Consume the backslash.
2. Consume the newline (LF or CRLF).
3. Consume any leading whitespace (spaces and tabs) on the next line.
4. Continue parsing the string content.

This implements the TOML v1.0.0 line-ending backslash feature that trims the continuation.

### 2.3 Parser: Style Tracking (TOML.Parser.pas)

When creating `TTOMLString` instances, set `FStyle` based on the delimiter used:
- `"` -> `tssBasic`
- `'` -> `tssLiteral`
- `"""` -> `tssMultilineBasic`
- `'''` -> `tssMultilineLiteral`

### 2.4 Serializer: Style-Aware Output (TOML.Serializer.pas)

Replace the current always-basic-string serialization with style-aware output:

- `tssBasic` -> `"value"` with escape sequences (current behavior).
- `tssLiteral` -> `'value'` with no escaping. Falls back to `tssBasic` if the value contains single quotes or control characters.
- `tssMultilineBasic` -> `"""` + newline + value + `"""` with escape sequences.
- `tssMultilineLiteral` -> `'''` + newline + value + `'''` with no escaping. Falls back to `tssMultilineBasic` if the value contains `'''`.

## Work Stream 3: Backward Compatibility

- `TTOMLTable.Items` property is preserved, pointing to `FIndex`. Existing code using `Table.Items['key']` or `Table.Items.Keys` continues to work.
- `TTOMLString.Create(AValue)` with one argument defaults to `tssBasic` style.
- Serialization output for documents without comments is unchanged (same key-value format, same ordering derived from body list which matches insertion order).

## Testing

### Comment Round-Trip Tests
- Parse TOML with standalone comments before keys, verify `TTOMLComment` entries in body at correct positions.
- Parse TOML with inline comments, verify `InlineComment` is set on correct values.
- Serialize parsed-with-comments document, verify comments appear in output.
- Full round-trip: parse -> serialize -> parse, verify structural equality including comments.

### Programmatic Comment API Tests
- `AddComment` then `Add`, serialize, verify comment appears before the key.
- Set `InlineComment`, serialize, verify it appears after the value.
- Multiple `AddComment` calls produce multiple ordered comment lines.

### Multiline String Tests
- Parse `"""` with line-ending backslash, verify whitespace trimming.
- Round-trip `"""` string, verify format preserved.
- Round-trip `'''` string, verify format preserved.
- Literal fallback: `tssLiteral` with `'` in value serializes as basic.

### Backward Compatibility Tests
- `Table.Items['key']` access works after restructure.
- Serialization output for comment-free documents is unchanged.

## Files Modified

| File | Changes |
|------|---------|
| `src/TOML.Types.pas` | `tvtComment`, `TTOMLComment`, `TTOMLStringStyle`, `TTOMLString.FStyle`, `TTOMLValue.FInlineComment`, `TTOMLTableEntry`, `TTOMLTable` body+index restructure |
| `src/TOML.Parser.pas` | Comment capture, inline comment capture, line-ending backslash, string style tracking |
| `src/TOML.Serializer.pas` | Body-order iteration, comment output, inline comment output, style-aware string serialization |
| `tests/delphi/` | New DUnitX test cases for all scenarios above |
