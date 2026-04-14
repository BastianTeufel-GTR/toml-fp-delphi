# Comment Support and Multiline String Fix Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add full round-trip comment preservation (standalone and inline) and fix multiline string serialization in the toml-fp library.

**Architecture:** Restructure `TTOMLTable` from a flat `TDictionary` to an ordered body list (`TList<TTOMLTableEntry>`) plus a dictionary index for fast lookups. Add `TTOMLComment` as a new value type for standalone comments, and `InlineComment` property on `TTOMLValue` for inline comments. Add `TTOMLStringStyle` enum to `TTOMLString` for round-trip multiline string preservation. Modify the lexer to emit comment tokens instead of discarding them, and the parser to capture them into the AST. Modify the serializer to iterate the body list and output comments.

**Tech Stack:** Delphi 12 (Win64), DUnitX, Free Pascal compiler conditionals maintained

**Spec:** `docs/superpowers/specs/2026-04-14-comment-support-multiline-fix-design.md`

---

## File Structure

| File | Action | Responsibility |
|------|--------|---------------|
| `src/TOML.Types.pas` | Modify | Add `tvtComment`, `TTOMLComment`, `TTOMLStringStyle`, `FInlineComment` on `TTOMLValue`, `TTOMLTableEntry`, restructure `TTOMLTable` with body list + index |
| `src/TOML.Parser.pas` | Modify | Emit `ttComment` tokens with text, capture standalone/inline comments in parser, line-ending backslash, string style tracking |
| `src/TOML.Serializer.pas` | Modify | Iterate body list, emit standalone comments, emit inline comments, style-aware string output |
| `src/TOML.pas` | Modify | Re-export `TTOMLComment`, `TTOMLStringStyle`, add `TOMLComment` helper |
| `tests/delphi/TOML.Tests.Comments.pas` | Create | DUnitX tests for comment parsing, serialization, round-trip, and programmatic API |
| `tests/delphi/TOML.Tests.MultilineStrings.pas` | Create | DUnitX tests for multiline string round-trip, line-ending backslash, literal fallback |
| `tests/delphi/TOML.Tests.dpr` | Modify | Register new test units |

---

### Task 1: Add TTOMLStringStyle enum and update TTOMLString

**Files:**
- Modify: `src/TOML.Types.pas:28-40` (type section, add enum before `TTOMLValueType`)
- Modify: `src/TOML.Types.pas:107-118` (TTOMLString class)
- Modify: `src/TOML.Types.pas:290-296` (TTOMLString implementation)

- [ ] **Step 1: Add TTOMLStringStyle enum to TOML.Types.pas**

In the `type` section, add the enum before the `TTOMLValueType` declaration (before line 31):

```pascal
TTOMLStringStyle = (
  tssBasic,             // "value"
  tssLiteral,           // 'value'
  tssMultilineBasic,    // """value"""
  tssMultilineLiteral   // '''value'''
);
```

- [ ] **Step 2: Add FStyle field and property to TTOMLString**

Update the `TTOMLString` class (lines 108-118) to:

```pascal
TTOMLString = class(TTOMLValue)
private
  FValue: string;
  FStyle: TTOMLStringStyle;
protected
  function GetAsString: string; override;
public
  constructor Create(const AValue: string; AStyle: TTOMLStringStyle = tssBasic);
  property Value: string read FValue write FValue;
  property Style: TTOMLStringStyle read FStyle write FStyle;
end;
```

- [ ] **Step 3: Update TTOMLString.Create implementation**

Replace the constructor implementation (lines 292-296) with:

```pascal
constructor TTOMLString.Create(const AValue: string; AStyle: TTOMLStringStyle = tssBasic);
begin
  inherited Create(tvtString);
  FValue := AValue;
  FStyle := AStyle;
end;
```

- [ ] **Step 4: Build to verify no regressions**

Run: Compile the project with the delphi-build MCP server.
Expected: Compiles successfully. The default parameter `tssBasic` ensures all existing `TTOMLString.Create('value')` calls remain valid.

- [ ] **Step 5: Commit**

```bash
git add src/TOML.Types.pas
git commit -m "Add TTOMLStringStyle enum and Style property to TTOMLString"
```

---

### Task 2: Add InlineComment to TTOMLValue and TTOMLComment class

**Files:**
- Modify: `src/TOML.Types.pas:31-40` (TTOMLValueType enum)
- Modify: `src/TOML.Types.pas:76-105` (TTOMLValue class)
- Modify: `src/TOML.Types.pas` (add TTOMLComment class after TTOMLString)
- Modify: `src/TOML.Types.pas` (add TTOMLComment implementation)

- [ ] **Step 1: Add tvtComment to TTOMLValueType**

Add `tvtComment` to the enum (after `tvtInlineTable`):

```pascal
TTOMLValueType = (
  tvtString,
  tvtInteger,
  tvtFloat,
  tvtBoolean,
  tvtDateTime,
  tvtArray,
  tvtTable,
  tvtInlineTable,
  tvtComment        // Comment value type (standalone full-line comment)
);
```

- [ ] **Step 2: Add FInlineComment field and property to TTOMLValue**

Add the field and property to `TTOMLValue` (after line 78, private section, and after line 104, public section):

```pascal
TTOMLValue = class
private
  FValueType: TTOMLValueType;
  FInlineComment: string;
protected
  function GetAsString: string; virtual;
  function GetAsInteger: Int64; virtual;
  function GetAsFloat: Double; virtual;
  function GetAsBoolean: Boolean; virtual;
  function GetAsDateTime: TDateTime; virtual;
  function GetAsArray: TTOMLArray; virtual;
  function GetAsTable: TTOMLTable; virtual;
public
  constructor Create(AType: TTOMLValueType);
  destructor Destroy; override;

  property ValueType: TTOMLValueType read FValueType;
  property AsString: string read GetAsString;
  property AsInteger: Int64 read GetAsInteger;
  property AsFloat: Double read GetAsFloat;
  property AsBoolean: Boolean read GetAsBoolean;
  property AsDateTime: TDateTime read GetAsDateTime;
  property AsArray: TTOMLArray read GetAsArray;
  property AsTable: TTOMLTable read GetAsTable;
  property InlineComment: string read FInlineComment write FInlineComment;
end;
```

- [ ] **Step 3: Add TTOMLComment class declaration**

Add the class after `TTOMLString` (after line 118):

```pascal
TTOMLComment = class(TTOMLValue)
private
  FText: string;
public
  constructor Create(const AText: string);
  property Text: string read FText write FText;
end;
```

- [ ] **Step 4: Add TTOMLComment.Create implementation**

Add after the `TTOMLString` implementation section (after line 301):

```pascal
{ TTOMLComment }

constructor TTOMLComment.Create(const AText: string);
begin
  inherited Create(tvtComment);
  FText := AText;
end;
```

- [ ] **Step 5: Build to verify compilation**

Run: Compile the project with the delphi-build MCP server.
Expected: Compiles successfully.

- [ ] **Step 6: Commit**

```bash
git add src/TOML.Types.pas
git commit -m "Add TTOMLComment class and InlineComment property to TTOMLValue"
```

---

### Task 3: Restructure TTOMLTable with ordered body list

**Files:**
- Modify: `src/TOML.Types.pas:58-73` (type declarations, add TTOMLTableEntry and list type)
- Modify: `src/TOML.Types.pas:204-231` (TTOMLTable class declaration)
- Modify: `src/TOML.Types.pas:399-478` (TTOMLTable implementation)

- [ ] **Step 1: Add TTOMLTableEntry record and list type**

Add after the `TTOMLTableDict` / `TTOMLValueList` declarations (after line 73, before `TTOMLValue` class):

```pascal
TTOMLTableEntry = record
  Key: string;
  Value: TTOMLValue;
end;

{$IF defined(FPC)}
TTOMLTableEntryList = specialize TList<TTOMLTableEntry>;
{$ELSE}
TTOMLTableEntryList = TList<TTOMLTableEntry>;
{$ENDIF}
```

- [ ] **Step 2: Update TTOMLTable class declaration**

Replace the `TTOMLTable` class (lines 204-231) with:

```pascal
TTOMLTable = class(TTOMLValue)
private
  FBody: TTOMLTableEntryList;
  FIndex: TTOMLTableDict;
protected
  function GetAsTable: TTOMLTable; override;
public
  constructor Create;
  destructor Destroy; override;

  procedure Add(const AKey: string; AValue: TTOMLValue);
  procedure AddComment(const AText: string);
  function TryGetValue(const AKey: string; out AValue: TTOMLValue): Boolean;

  property Items: TTOMLTableDict read FIndex;
  property Body: TTOMLTableEntryList read FBody;
end;
```

- [ ] **Step 3: Rewrite TTOMLTable implementation**

Replace the `TTOMLTable` implementation section (lines 399-478) with:

```pascal
{ TTOMLTable }

constructor TTOMLTable.Create;
begin
  inherited Create(tvtTable);
  FBody := TTOMLTableEntryList.Create;
  FIndex := TTOMLTableDict.Create;
end;

destructor TTOMLTable.Destroy;
var
  i: Integer;
begin
  // Free all values via the body list (single ownership)
  for i := 0 to FBody.Count - 1 do
    FBody[i].Value.Free;
  FBody.Free;
  // FIndex holds the same references — do NOT free values again
  FIndex.Free;
  inherited Destroy;
end;

procedure TTOMLTable.Add(const AKey: string; AValue: TTOMLValue);
var
  ExistingValue: TTOMLValue;
  MergeSource: TTOMLTable;
  MergeKeys: TStringList;
  MergeKey: string;
  MergeValue: TTOMLValue;
  i: Integer;
  Entry: TTOMLTableEntry;
begin
  if FIndex.TryGetValue(AKey, ExistingValue) then
  begin
    // Both existing and new are tables: merge new entries into existing
    if (ExistingValue is TTOMLTable) and (AValue is TTOMLTable) then
    begin
      MergeSource := TTOMLTable(AValue);
      MergeKeys := TStringList.Create;
      try
        for MergeKey in MergeSource.Items.Keys do
          MergeKeys.Add(MergeKey);
        for i := 0 to MergeKeys.Count - 1 do
        begin
          MergeKey := MergeKeys[i];
          MergeValue := MergeSource.Items[MergeKey];
          MergeSource.FIndex.Remove(MergeKey);
          // Also remove from source body to prevent double-free
          MergeSource.RemoveFromBody(MergeKey);
          try
            TTOMLTable(ExistingValue).Add(MergeKey, MergeValue);
          except
            MergeSource.FIndex.AddOrSetValue(MergeKey, MergeValue);
            // Re-add to source body
            Entry.Key := MergeKey;
            Entry.Value := MergeValue;
            MergeSource.FBody.Add(Entry);
            raise;
          end;
        end;
      finally
        MergeKeys.Free;
      end;
      MergeSource.Free;
    end
    else
      raise ETOMLParserException.CreateFmt('Duplicate key "%s" found', [AKey]);
  end
  else
  begin
    Entry.Key := AKey;
    Entry.Value := AValue;
    FBody.Add(Entry);
    FIndex.AddOrSetValue(AKey, AValue);
  end;
end;

procedure TTOMLTable.AddComment(const AText: string);
var
  Entry: TTOMLTableEntry;
begin
  Entry.Key := '';
  Entry.Value := TTOMLComment.Create(AText);
  FBody.Add(Entry);
end;

function TTOMLTable.TryGetValue(const AKey: string; out AValue: TTOMLValue): Boolean;
begin
  Result := FIndex.TryGetValue(AKey, AValue);
end;

function TTOMLTable.GetAsTable: TTOMLTable;
begin
  Result := Self;
end;
```

- [ ] **Step 4: Add RemoveFromBody private helper**

Add to the `TTOMLTable` class declaration in the private section:

```pascal
private
  FBody: TTOMLTableEntryList;
  FIndex: TTOMLTableDict;
  procedure RemoveFromBody(const AKey: string);
```

And the implementation:

```pascal
procedure TTOMLTable.RemoveFromBody(const AKey: string);
var
  i: Integer;
begin
  for i := FBody.Count - 1 downto 0 do
    if FBody[i].Key = AKey then
    begin
      // Do NOT free the value — ownership is being transferred
      FBody.Delete(i);
      Exit;
    end;
end;
```

- [ ] **Step 5: Build to verify compilation**

Run: Compile the project with the delphi-build MCP server.
Expected: Compiles successfully. All existing code using `Table.Items['key']` or `Table.TryGetValue` continues to work because `Items` still returns the dictionary.

- [ ] **Step 6: Commit**

```bash
git add src/TOML.Types.pas
git commit -m "Restructure TTOMLTable with ordered body list and dictionary index"
```

---

### Task 4: Update TOML.pas facade with new type re-exports

**Files:**
- Modify: `src/TOML.pas:38-52` (type re-exports)
- Modify: `src/TOML.pas:54-94` (helper functions)
- Modify: `src/TOML.pas:138-175` (helper function implementations)

- [ ] **Step 1: Add re-exports for new types**

Add after the existing type re-exports (after `TTOMLTable` on line 47):

```pascal
TTOMLComment = TOML.Types.TTOMLComment;
TTOMLStringStyle = TOML.Types.TTOMLStringStyle;
TTOMLTableEntry = TOML.Types.TTOMLTableEntry;
TTOMLTableEntryList = TOML.Types.TTOMLTableEntryList;
```

- [ ] **Step 2: Add TOMLComment helper function declaration**

Add after the `TOMLTable` function declaration (after line 94):

```pascal
{ Creates a new TOML comment value
  @param AText The comment text (without # prefix)
  @returns A new TTOMLComment instance }
function TOMLComment(const AText: string): TTOMLComment;
```

- [ ] **Step 3: Add TOMLComment helper function implementation**

Add after the `TOMLTable` function implementation (after line 175):

```pascal
function TOMLComment(const AText: string): TTOMLComment;
begin
  Result := TTOMLComment.Create(AText);
end;
```

- [ ] **Step 4: Build to verify compilation**

Run: Compile the project with the delphi-build MCP server.
Expected: Compiles successfully.

- [ ] **Step 5: Commit**

```bash
git add src/TOML.pas
git commit -m "Add TTOMLComment and TTOMLStringStyle re-exports to TOML facade unit"
```

---

### Task 5: Update lexer to emit comment tokens

**Files:**
- Modify: `src/TOML.Parser.pas:361-375` (SkipWhitespace method)
- Modify: `src/TOML.Parser.pas:828-931` (NextToken method)

- [ ] **Step 1: Modify SkipWhitespace to only skip whitespace, not comments**

Replace `SkipWhitespace` (lines 361-375) with:

```pascal
procedure TTOMLLexer.SkipWhitespace;
begin
  while not IsAtEnd do
  begin
    case Peek of
      ' ', #9: Advance;
      else
        Break;
    end;
  end;
end;
```

- [ ] **Step 2: Add comment token handling to NextToken**

In the `NextToken` method (line 828), add a case for `#` in the `case Peek of` block (before the newline case at line 881). Insert after the `}` case (line 880):

```pascal
'#': begin
  Advance; // Skip the # character
  // Skip optional space after #
  Result.Value := '';
  while not IsAtEnd and (Peek <> #10) and (Peek <> #13) do
    Result.Value := Result.Value + Advance;
  // Trim leading space from comment text
  if (Length(Result.Value) > 0) and (Result.Value[1] = ' ') then
    Delete(Result.Value, 1, 1);
  Result.TokenType := ttComment;
  Result.Line := FLine;
  Result.Column := FColumn;
end;
```

- [ ] **Step 3: Build to verify compilation**

Run: Compile the project with the delphi-build MCP server.
Expected: Compiles successfully. Note: existing tests may now fail because the parser does not yet handle `ttComment` tokens. That is expected and will be fixed in Task 6.

- [ ] **Step 4: Commit**

```bash
git add src/TOML.Parser.pas
git commit -m "Update lexer to emit comment tokens instead of discarding them"
```

---

### Task 6: Update parser to capture comments

**Files:**
- Modify: `src/TOML.Parser.pas:1375-1522` (Parse method)
- Modify: `src/TOML.Parser.pas:1335-1373` (ParseKeyValue method)

- [ ] **Step 1: Capture standalone comments in the Parse method**

In the `Parse` method (line 1375), the main `case` statement currently handles `ttLBracket`, `ttIdentifier`, `ttString`, `ttNewLine`. Add a case for `ttComment` before the `ttNewLine` case (before line 1507):

```pascal
ttComment:
begin
  CurrentTable.AddComment(FCurrentToken.Value);
  Advance;
end;
```

- [ ] **Step 2: Capture inline comments after key-value pairs**

In the `Parse` method, after a key-value pair is added to the table (after line 1493 `CurrentTable.Add(KeyPair.Key, KeyPair.Value);`), check for an inline comment before consuming the newline. Modify the `ttIdentifier, ttString:` case block to:

```pascal
ttIdentifier, ttString:
begin
  try
    KeyPair := ParseKeyValue;
    try
      CurrentTable.Add(KeyPair.Key, KeyPair.Value);
      // Check for inline comment after value
      if FCurrentToken.TokenType = ttComment then
      begin
        KeyPair.Value.InlineComment := FCurrentToken.Value;
        Advance;
      end;
    except
      KeyPair.Value.Free;
      raise;
    end;
  except
    on E: ETOMLParserException do
      raise;
    on E: Exception do
      raise ETOMLParserException.CreateFmt('Error adding key-value pair: %s at line %d, column %d',
        [E.Message, FCurrentToken.Line, FCurrentToken.Column]);
  end;
end;
```

- [ ] **Step 3: Also capture inline comments after table headers**

In the `Parse` method, after `Expect(ttRBracket)` for both regular and array-of-tables headers, the parser should skip any inline comment. After the second `Expect(ttRBracket)` for array-of-tables (line 1414), and after `Expect(ttRBracket)` for regular tables (line 1412), add:

```pascal
Expect(ttRBracket);
if IsArrayOfTables then
  Expect(ttRBracket);

// Skip inline comment after table header (out of scope for preservation)
if FCurrentToken.TokenType = ttComment then
  Advance;
```

- [ ] **Step 4: Build to verify compilation**

Run: Compile the project with the delphi-build MCP server.
Expected: Compiles successfully.

- [ ] **Step 5: Commit**

```bash
git add src/TOML.Parser.pas
git commit -m "Capture standalone and inline comments in parser"
```

---

### Task 7: Implement line-ending backslash for multiline basic strings

**Files:**
- Modify: `src/TOML.Parser.pas:448-508` (escape sequence handling in ScanString)

- [ ] **Step 1: Add line-ending backslash handling**

In `ScanString`, inside the escape sequence `case Peek of` block (line 451), add a check for newline after backslash. Insert before the `else raise` at line 506:

```pascal
#10, #13: begin
  // Line-ending backslash: trim newline and following whitespace
  if IsMultiline then
  begin
    if Peek = #13 then Advance; // Skip CR
    if Peek = #10 then Advance; // Skip LF
    // Skip leading whitespace on next line
    while not IsAtEnd and ((Peek = ' ') or (Peek = #9)) do
      Advance;
    Continue;
  end
  else
    raise ETOMLParserException.Create('Invalid escape sequence');
end;
```

The full case block for escape sequences should now include this before the final `else`:

```pascal
case Peek of
  'n': TempValue := TempValue + #10;
  't': TempValue := TempValue + #9;
  'r': TempValue := TempValue + #13;
  '\': TempValue := TempValue + '\';
  '"': TempValue := TempValue + '"';
  '''': TempValue := TempValue + '''';
  'u', 'U': begin
    // ... existing unicode handling ...
  end;
  #10, #13: begin
    // Line-ending backslash: trim newline and following whitespace
    if IsMultiline then
    begin
      if Peek = #13 then Advance;
      if Peek = #10 then Advance;
      while not IsAtEnd and ((Peek = ' ') or (Peek = #9)) do
        Advance;
      Continue;
    end
    else
      raise ETOMLParserException.Create('Invalid escape sequence');
  end;
  else raise ETOMLParserException.Create('Invalid escape sequence');
end;
```

- [ ] **Step 2: Build to verify compilation**

Run: Compile the project with the delphi-build MCP server.
Expected: Compiles successfully.

- [ ] **Step 3: Commit**

```bash
git add src/TOML.Parser.pas
git commit -m "Implement line-ending backslash for multiline basic strings"
```

---

### Task 8: Track string style in parser

**Files:**
- Modify: `src/TOML.Parser.pas:392-528` (ScanString — add style tracking)
- Modify: `src/TOML.Parser.pas:1022-1026` (ParseString — pass style to constructor)

- [ ] **Step 1: Track style in ScanString**

The `ScanString` function currently returns a `TToken` but tokens don't carry style info. The simplest approach: encode the style in the token value or add a field. Since `TToken` is a simple record shared across the codebase, the cleanest approach is to have `ParseString` determine the style from what the lexer already computes.

However, the lexer's `ScanString` has the `IsMultiline` and `IsLiteral` local variables but these are not returned. Add a way to communicate this. The simplest: encode the style as a prefix in `TToken.Value` that `ParseString` can strip, **or** add a field to `TToken`.

Add a `StringStyle` field to `TToken`:

```pascal
TToken = record
  TokenType: TTokenType;
  Value: string;
  Line: Integer;
  Column: Integer;
  StringStyle: TTOMLStringStyle;
end;
```

- [ ] **Step 2: Set StringStyle in ScanString**

At the end of `ScanString`, just before the `Result.TokenType := ttString;` line (line 514), set the style:

```pascal
if IsMultiline and IsLiteral then
  Result.StringStyle := tssMultilineLiteral
else if IsMultiline then
  Result.StringStyle := tssMultilineBasic
else if IsLiteral then
  Result.StringStyle := tssLiteral
else
  Result.StringStyle := tssBasic;

Result.TokenType := ttString;
Result.Value := TempValue;
Result.Line := FLine;
Result.Column := StartColumn;
```

- [ ] **Step 3: Update ParseString to pass style**

Replace `ParseString` (lines 1022-1026) with:

```pascal
function TTOMLParser.ParseString: TTOMLString;
begin
  Result := TTOMLString.Create(FCurrentToken.Value, FCurrentToken.StringStyle);
  Advance;
end;
```

- [ ] **Step 4: Build to verify compilation**

Run: Compile the project with the delphi-build MCP server.
Expected: Compiles successfully.

- [ ] **Step 5: Commit**

```bash
git add src/TOML.Parser.pas
git commit -m "Track string style (basic/literal/multiline) in lexer and parser"
```

---

### Task 9: Update serializer to iterate body list and output comments

**Files:**
- Modify: `src/TOML.Serializer.pas:331-358` (WriteValue — handle tvtComment)
- Modify: `src/TOML.Serializer.pas:360-453` (WriteTable — iterate body)

- [ ] **Step 1: Add tvtComment case to WriteValue**

In `WriteValue` (line 331), add a case for `tvtComment` (before the final `end;`):

```pascal
tvtComment:
begin
  FStringBuilder.Append('# ');
  FStringBuilder.Append(TTOMLComment(AValue).Text);
end;
```

- [ ] **Step 2: Add inline comment helper method**

Add a new private method declaration to `TTOMLSerializer` (after line 85):

```pascal
procedure WriteInlineComment(const AValue: TTOMLValue);
```

And the implementation (after `WriteDateTime`, around line 291):

```pascal
procedure TTOMLSerializer.WriteInlineComment(const AValue: TTOMLValue);
begin
  if AValue.InlineComment <> '' then
  begin
    FStringBuilder.Append(' # ');
    FStringBuilder.Append(AValue.InlineComment);
  end;
end;
```

- [ ] **Step 3: Rewrite WriteTable to iterate body list**

Replace the `WriteTable` method (lines 360-453) with:

```pascal
procedure TTOMLSerializer.WriteTable(const ATable: TTOMLTable; const AInline: Boolean = False);
var
  First: Boolean;
  Pair: TTOMLKeyValuePair;
  Entry: TTOMLTableEntry;
  SubTable: TTOMLTable;
  i, j: Integer;
  ArrayValue: TTOMLArray;
  AllTables: Boolean;
begin
  if AInline then
  begin
    // Write inline table format: { key1 = value1, key2 = value2 }
    FStringBuilder.Append('{');
    First := True;

    for i := 0 to ATable.Body.Count - 1 do
    begin
      Entry := ATable.Body[i];
      // Skip comments in inline tables
      if Entry.Value is TTOMLComment then
        Continue;

      if not First then
        FStringBuilder.Append(', ')
      else
        First := False;

      WriteKey(Entry.Key);
      FStringBuilder.Append(' = ');
      WriteValue(Entry.Value);
    end;

    FStringBuilder.Append('}');
  end
  else
  begin
    // First pass: write all non-table, non-array-of-tables values and comments
    for i := 0 to ATable.Body.Count - 1 do
    begin
      Entry := ATable.Body[i];

      // Standalone comments
      if Entry.Value is TTOMLComment then
      begin
        WriteValue(Entry.Value);
        WriteLine;
        Continue;
      end;

      // Skip sub-tables and arrays-of-tables (handled in second pass)
      if Entry.Value.ValueType = tvtTable then
        Continue;
      if (Entry.Value.ValueType = tvtArray) and (Entry.Value.AsArray.Count > 0) and
         (Entry.Value.AsArray.GetItem(0).ValueType = tvtTable) then
        Continue;

      WriteKey(Entry.Key);
      FStringBuilder.Append(' = ');
      WriteValue(Entry.Value);
      WriteInlineComment(Entry.Value);
      WriteLine;
    end;

    // Second pass: write arrays of tables, then sub-tables (preserving body order)
    for i := 0 to ATable.Body.Count - 1 do
    begin
      Entry := ATable.Body[i];

      // Skip comments and scalar values (already written)
      if Entry.Value is TTOMLComment then
        Continue;

      if (Entry.Value.ValueType = tvtArray) and (Entry.Value.AsArray.Count > 0) then
      begin
        ArrayValue := Entry.Value.AsArray;

        AllTables := True;
        for j := 0 to ArrayValue.Count - 1 do
        begin
          if ArrayValue.GetItem(j).ValueType <> tvtTable then
          begin
            AllTables := False;
            Break;
          end;
        end;

        if AllTables then
        begin
          for j := 0 to ArrayValue.Count - 1 do
          begin
            if j > 0 then
              WriteLine;
            WriteLine('[[' + BuildPath(Entry.Key) + ']]');
            FCurrentPath.Add(Entry.Key);
            WriteTable(ArrayValue.GetItem(j).AsTable);
            FCurrentPath.Delete(FCurrentPath.Count - 1);
          end;
          Continue;
        end;
      end;

      if Entry.Value.ValueType = tvtTable then
      begin
        SubTable := Entry.Value.AsTable;
        WriteLine;
        WriteLine('[' + BuildPath(Entry.Key) + ']');
        if SubTable.Body.Count > 0 then
        begin
          FCurrentPath.Add(Entry.Key);
          WriteTable(SubTable);
          FCurrentPath.Delete(FCurrentPath.Count - 1);
        end;
      end;
    end;
  end;
end;
```

Note the key changes from the original:
- Iterates `ATable.Body` instead of `ATable.Items`
- Handles `TTOMLComment` entries
- Calls `WriteInlineComment` after each key-value pair
- Uses `SubTable.Body.Count` instead of `SubTable.Items.Count`

- [ ] **Step 4: Build to verify compilation**

Run: Compile the project with the delphi-build MCP server.
Expected: Compiles successfully.

- [ ] **Step 5: Commit**

```bash
git add src/TOML.Serializer.pas
git commit -m "Update serializer to iterate body list and output comments"
```

---

### Task 10: Implement style-aware string serialization

**Files:**
- Modify: `src/TOML.Serializer.pas:260-285` (WriteString method)
- Modify: `src/TOML.Serializer.pas:331-358` (WriteValue — pass style info)

- [ ] **Step 1: Rename WriteString to WriteBasicString and add style-aware WriteStringValue**

Keep the existing `WriteString` for basic string output (used by `WriteKey` too) but add a new method for style-aware value output. Add a new private method declaration:

```pascal
procedure WriteStringValue(const AValue: TTOMLString);
```

- [ ] **Step 2: Implement WriteStringValue**

Add the implementation after `WriteString`:

```pascal
procedure TTOMLSerializer.WriteStringValue(const AValue: TTOMLString);
var
  i: Integer;
  C: Char;
  S: string;
begin
  S := AValue.Value;
  case AValue.Style of
    tssBasic:
      WriteString(S);

    tssLiteral:
    begin
      // Fall back to basic if value contains single quote or control chars
      if (Pos('''', S) > 0) then
        WriteString(S)
      else
      begin
        // Check for control characters
        for i := 1 to Length(S) do
          if Ord(S[i]) < 32 then
          begin
            WriteString(S);
            Exit;
          end;
        FStringBuilder.Append('''');
        FStringBuilder.Append(S);
        FStringBuilder.Append('''');
      end;
    end;

    tssMultilineBasic:
    begin
      FStringBuilder.Append('"""');
      FStringBuilder.AppendLine;
      for i := 1 to Length(S) do
      begin
        C := S[i];
        case C of
          '\': FStringBuilder.Append('\\');
          '"': begin
            // Only escape if three consecutive quotes
            if (i + 2 <= Length(S)) and (S[i+1] = '"') and (S[i+2] = '"') then
              FStringBuilder.Append('\"')
            else
              FStringBuilder.Append('"');
          end;
          #8:  FStringBuilder.Append('\b');
          #9:  FStringBuilder.Append(#9);   // Tab is allowed literally in multiline
          #10: FStringBuilder.Append(#10);  // Newline is allowed literally
          #13: FStringBuilder.Append(#13);  // CR is allowed literally
          else
            if C < #32 then
              FStringBuilder.AppendFormat('\u%.4x', [Ord(C)])
            else
              FStringBuilder.Append(C);
        end;
      end;
      FStringBuilder.Append('"""');
    end;

    tssMultilineLiteral:
    begin
      // Fall back to multiline basic if value contains '''
      if Pos('''''''''', S) > 0 then
      begin
        // Output as multiline basic instead
        FStringBuilder.Append('"""');
        FStringBuilder.AppendLine;
        FStringBuilder.Append(EscapeTomlString(S));
        FStringBuilder.Append('"""');
      end
      else
      begin
        FStringBuilder.Append('''''''''');
        FStringBuilder.AppendLine;
        FStringBuilder.Append(S);
        FStringBuilder.Append('''''''''');
      end;
    end;
  end;
end;
```

Note: The triple single-quote literals in the source code are `'''` (three single-quote characters). In the Delphi source this is written as `''''''''` (escaped single quotes).

- [ ] **Step 3: Update WriteValue to use WriteStringValue**

In `WriteValue` (line 331), change the `tvtString` case from:

```pascal
tvtString:
  WriteString(AValue.AsString);
```

to:

```pascal
tvtString:
  WriteStringValue(TTOMLString(AValue));
```

- [ ] **Step 4: Build to verify compilation**

Run: Compile the project with the delphi-build MCP server.
Expected: Compiles successfully.

- [ ] **Step 5: Commit**

```bash
git add src/TOML.Serializer.pas
git commit -m "Implement style-aware string serialization for multiline and literal strings"
```

---

### Task 11: Create comment tests

**Files:**
- Create: `tests/delphi/TOML.Tests.Comments.pas`
- Modify: `tests/delphi/TOML.Tests.dpr` (register new unit)

- [ ] **Step 1: Write the comment test unit**

Create `tests/delphi/TOML.Tests.Comments.pas`:

```pascal
{*******************************************************************************
  Unit Name: TOML.Tests.Comments
  Purpose: DUnitX tests for TOML comment parsing, serialization, and round-trip

  Copyright (c) 1984-2026 GTR mbH

  Dependencies:
    - DUnitX.TestFramework
    - TOML, TOML.Types, TOML.Parser, TOML.Serializer
*******************************************************************************}
unit TOML.Tests.Comments;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  TOML,
  TOML.Types;

type
  [TestFixture]
  TTOMLCommentTests = class
  published
    [Test]
    procedure Parse_StandaloneComment_CapturedInBody;

    [Test]
    procedure Parse_MultipleStandaloneComments_PreserveOrder;

    [Test]
    procedure Parse_InlineComment_CapturedOnValue;

    [Test]
    procedure Serialize_StandaloneComment_OutputBeforeKey;

    [Test]
    procedure Serialize_InlineComment_OutputAfterValue;

    [Test]
    procedure RoundTrip_StandaloneAndInline_Preserved;

    [Test]
    procedure API_AddComment_AppearsBeforeNextKey;

    [Test]
    procedure API_MultipleAddComment_AllAppearInOrder;

    [Test]
    procedure API_InlineComment_AppearsAfterValue;

    [Test]
    procedure Parse_CommentBeforeTableHeader_Captured;

    [Test]
    procedure Parse_NoComment_InlineCommentEmpty;

    [Test]
    procedure Serialize_NoComments_OutputUnchanged;
  end;

implementation

uses
  TOML.Parser, TOML.Serializer;

procedure TTOMLCommentTests.Parse_StandaloneComment_CapturedInBody;
var
  Doc: TTOMLTable;
begin
  Doc := ParseTOML(
    '# this is a comment' + #10 +
    'key = "value"' + #10
  );
  try
    Assert.AreEqual(2, Doc.Body.Count, 'Body should have 2 entries (comment + key)');
    Assert.IsTrue(Doc.Body[0].Value is TTOMLComment, 'First entry should be a comment');
    Assert.AreEqual('this is a comment', TTOMLComment(Doc.Body[0].Value).Text);
    Assert.AreEqual('key', Doc.Body[1].Key);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLCommentTests.Parse_MultipleStandaloneComments_PreserveOrder;
var
  Doc: TTOMLTable;
begin
  Doc := ParseTOML(
    '# first comment' + #10 +
    '# second comment' + #10 +
    '# third comment' + #10 +
    'key = 42' + #10
  );
  try
    Assert.AreEqual(4, Doc.Body.Count, 'Body should have 4 entries');
    Assert.AreEqual('first comment', TTOMLComment(Doc.Body[0].Value).Text);
    Assert.AreEqual('second comment', TTOMLComment(Doc.Body[1].Value).Text);
    Assert.AreEqual('third comment', TTOMLComment(Doc.Body[2].Value).Text);
    Assert.AreEqual('key', Doc.Body[3].Key);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLCommentTests.Parse_InlineComment_CapturedOnValue;
var
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  Doc := ParseTOML('key = "value" # inline comment' + #10);
  try
    Assert.IsTrue(Doc.TryGetValue('key', Value));
    Assert.AreEqual('inline comment', Value.InlineComment);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLCommentTests.Serialize_StandaloneComment_OutputBeforeKey;
var
  Table: TTOMLTable;
  Output: string;
begin
  Table := TTOMLTable.Create;
  try
    Table.AddComment('default resolution');
    Table.Add('resolution', TTOMLFloat.Create(0.5));
    Output := SerializeTOML(Table);
    Assert.Contains(Output, '# default resolution');
    // Comment should appear before the key
    Assert.IsTrue(Pos('# default resolution', Output) < Pos('resolution = ', Output),
      'Comment should appear before the key-value pair');
  finally
    Table.Free;
  end;
end;

procedure TTOMLCommentTests.Serialize_InlineComment_OutputAfterValue;
var
  Table: TTOMLTable;
  FloatVal: TTOMLFloat;
  Output: string;
begin
  Table := TTOMLTable.Create;
  try
    FloatVal := TTOMLFloat.Create(0.5);
    FloatVal.InlineComment := 'in millimeters';
    Table.Add('resolution', FloatVal);
    Output := SerializeTOML(Table);
    Assert.Contains(Output, '0.5 # in millimeters');
  finally
    Table.Free;
  end;
end;

procedure TTOMLCommentTests.RoundTrip_StandaloneAndInline_Preserved;
var
  Input: string;
  Doc: TTOMLTable;
  Output: string;
  Doc2: TTOMLTable;
begin
  Input :=
    '# default STL resolution' + #10 +
    '# dimension in mm' + #10 +
    'stl_resolution = 0.5 # accuracy setting' + #10;

  Doc := ParseTOML(Input);
  try
    Output := SerializeTOML(Doc);
    Assert.Contains(Output, '# default STL resolution');
    Assert.Contains(Output, '# dimension in mm');
    Assert.Contains(Output, '# accuracy setting');

    // Second pass: parse the output again
    Doc2 := ParseTOML(Output);
    try
      Assert.AreEqual(3, Doc2.Body.Count, 'Re-parsed body should have 3 entries');
      Assert.IsTrue(Doc2.Body[0].Value is TTOMLComment);
      Assert.IsTrue(Doc2.Body[1].Value is TTOMLComment);
      Assert.AreEqual('accuracy setting', Doc2.Body[2].Value.InlineComment);
    finally
      Doc2.Free;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TTOMLCommentTests.API_AddComment_AppearsBeforeNextKey;
var
  Table: TTOMLTable;
  Output: string;
begin
  Table := TTOMLTable.Create;
  try
    Table.Add('name', TTOMLString.Create('test'));
    Table.AddComment('settings below');
    Table.Add('value', TTOMLInteger.Create(42));
    Output := SerializeTOML(Table);
    Assert.IsTrue(Pos('name', Output) < Pos('# settings below', Output),
      'Comment should appear after name');
    Assert.IsTrue(Pos('# settings below', Output) < Pos('value', Output),
      'Comment should appear before value');
  finally
    Table.Free;
  end;
end;

procedure TTOMLCommentTests.API_MultipleAddComment_AllAppearInOrder;
var
  Table: TTOMLTable;
  Output: string;
  Pos1, Pos2, Pos3: Integer;
begin
  Table := TTOMLTable.Create;
  try
    Table.AddComment('line one');
    Table.AddComment('line two');
    Table.AddComment('line three');
    Table.Add('key', TTOMLString.Create('val'));
    Output := SerializeTOML(Table);
    Pos1 := Pos('# line one', Output);
    Pos2 := Pos('# line two', Output);
    Pos3 := Pos('# line three', Output);
    Assert.IsTrue((Pos1 > 0) and (Pos2 > Pos1) and (Pos3 > Pos2),
      'Comments should appear in insertion order');
  finally
    Table.Free;
  end;
end;

procedure TTOMLCommentTests.API_InlineComment_AppearsAfterValue;
var
  Table: TTOMLTable;
  IntVal: TTOMLInteger;
  Output: string;
begin
  Table := TTOMLTable.Create;
  try
    IntVal := TTOMLInteger.Create(100);
    IntVal.InlineComment := 'max count';
    Table.Add('limit', IntVal);
    Output := SerializeTOML(Table);
    Assert.Contains(Output, 'limit = 100 # max count');
  finally
    Table.Free;
  end;
end;

procedure TTOMLCommentTests.Parse_CommentBeforeTableHeader_Captured;
var
  Doc: TTOMLTable;
  SubTable: TTOMLTable;
  SubValue: TTOMLValue;
begin
  Doc := ParseTOML(
    '# root comment' + #10 +
    '[section]' + #10 +
    '# section comment' + #10 +
    'key = "value"' + #10
  );
  try
    // Root comment should be in root body
    Assert.IsTrue(Doc.Body[0].Value is TTOMLComment);
    Assert.AreEqual('root comment', TTOMLComment(Doc.Body[0].Value).Text);

    // Section comment should be in sub-table body
    Assert.IsTrue(Doc.TryGetValue('section', SubValue));
    SubTable := SubValue.AsTable;
    Assert.AreEqual(2, SubTable.Body.Count, 'Sub-table should have comment + key');
    Assert.IsTrue(SubTable.Body[0].Value is TTOMLComment);
    Assert.AreEqual('section comment', TTOMLComment(SubTable.Body[0].Value).Text);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLCommentTests.Parse_NoComment_InlineCommentEmpty;
var
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  Doc := ParseTOML('key = "value"' + #10);
  try
    Assert.IsTrue(Doc.TryGetValue('key', Value));
    Assert.AreEqual('', Value.InlineComment);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLCommentTests.Serialize_NoComments_OutputUnchanged;
var
  Table: TTOMLTable;
  Output: string;
begin
  Table := TTOMLTable.Create;
  try
    Table.Add('name', TTOMLString.Create('test'));
    Table.Add('count', TTOMLInteger.Create(5));
    Output := SerializeTOML(Table);
    Assert.AreEqual(-1, Pos('#', Output), 'No # should appear in output without comments');
  finally
    Table.Free;
  end;
end;

end.
```

- [ ] **Step 2: Build to verify test unit compiles**

Run: Compile the project with the delphi-build MCP server.
Expected: Compiles successfully.

- [ ] **Step 3: Commit**

```bash
git add tests/delphi/TOML.Tests.Comments.pas
git commit -m "Add DUnitX comment tests for parsing, serialization, and round-trip"
```

---

### Task 12: Create multiline string tests

**Files:**
- Create: `tests/delphi/TOML.Tests.MultilineStrings.pas`

- [ ] **Step 1: Write the multiline string test unit**

Create `tests/delphi/TOML.Tests.MultilineStrings.pas`:

```pascal
{*******************************************************************************
  Unit Name: TOML.Tests.MultilineStrings
  Purpose: DUnitX tests for multiline string round-trip, line-ending backslash,
           and literal string fallback

  Copyright (c) 1984-2026 GTR mbH

  Dependencies:
    - DUnitX.TestFramework
    - TOML, TOML.Types, TOML.Parser, TOML.Serializer
*******************************************************************************}
unit TOML.Tests.MultilineStrings;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  TOML,
  TOML.Types;

type
  [TestFixture]
  TTOMLMultilineStringTests = class
  published
    [Test]
    procedure Parse_MultilineBasic_PreservesStyle;

    [Test]
    procedure Parse_MultilineLiteral_PreservesStyle;

    [Test]
    procedure Parse_BasicString_StyleIsBasic;

    [Test]
    procedure Parse_LiteralString_StyleIsLiteral;

    [Test]
    procedure RoundTrip_MultilineBasic_PreservesTripleQuotes;

    [Test]
    procedure RoundTrip_MultilineLiteral_PreservesTripleSingleQuotes;

    [Test]
    procedure Parse_LineEndingBackslash_TrimsWhitespace;

    [Test]
    procedure Parse_LineEndingBackslash_MultipleContinuations;

    [Test]
    procedure Serialize_LiteralWithSingleQuote_FallsBackToBasic;

    [Test]
    procedure Serialize_LiteralWithoutSpecialChars_UsesLiteralStyle;

    [Test]
    procedure RoundTrip_BasicString_StaysBasic;
  end;

implementation

uses
  TOML.Parser, TOML.Serializer;

procedure TTOMLMultilineStringTests.Parse_MultilineBasic_PreservesStyle;
var
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  Doc := ParseTOML(
    'key = """' + #10 +
    'hello' + #10 +
    'world"""' + #10
  );
  try
    Assert.IsTrue(Doc.TryGetValue('key', Value));
    Assert.AreEqual(tssMultilineBasic, TTOMLString(Value).Style);
    Assert.AreEqual('hello' + #10 + 'world', Value.AsString);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.Parse_MultilineLiteral_PreservesStyle;
var
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  Doc := ParseTOML(
    'key = ''''''''' + #10 +
    'hello' + #10 +
    'world''''''''' + #10
  );
  try
    Assert.IsTrue(Doc.TryGetValue('key', Value));
    Assert.AreEqual(tssMultilineLiteral, TTOMLString(Value).Style);
    Assert.AreEqual('hello' + #10 + 'world', Value.AsString);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.Parse_BasicString_StyleIsBasic;
var
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  Doc := ParseTOML('key = "hello"' + #10);
  try
    Assert.IsTrue(Doc.TryGetValue('key', Value));
    Assert.AreEqual(tssBasic, TTOMLString(Value).Style);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.Parse_LiteralString_StyleIsLiteral;
var
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  Doc := ParseTOML('key = ''hello''' + #10);
  try
    Assert.IsTrue(Doc.TryGetValue('key', Value));
    Assert.AreEqual(tssLiteral, TTOMLString(Value).Style);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.RoundTrip_MultilineBasic_PreservesTripleQuotes;
var
  Doc: TTOMLTable;
  Output: string;
begin
  Doc := ParseTOML(
    'key = """' + #10 +
    'line1' + #10 +
    'line2"""' + #10
  );
  try
    Output := SerializeTOML(Doc);
    Assert.Contains(Output, '"""');
  finally
    Doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.RoundTrip_MultilineLiteral_PreservesTripleSingleQuotes;
var
  Doc: TTOMLTable;
  Output: string;
begin
  Doc := ParseTOML(
    'key = ''''''''' + #10 +
    'line1' + #10 +
    'line2''''''''' + #10
  );
  try
    Output := SerializeTOML(Doc);
    Assert.Contains(Output, '''''''');
  finally
    Doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.Parse_LineEndingBackslash_TrimsWhitespace;
var
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  Doc := ParseTOML(
    'key = """' + #10 +
    'hello \' + #10 +
    '       world"""' + #10
  );
  try
    Assert.IsTrue(Doc.TryGetValue('key', Value));
    Assert.AreEqual('hello world', Value.AsString);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.Parse_LineEndingBackslash_MultipleContinuations;
var
  Doc: TTOMLTable;
  Value: TTOMLValue;
begin
  Doc := ParseTOML(
    'key = """' + #10 +
    'The quick brown \' + #10 +
    '  fox jumps over \' + #10 +
    '  the lazy dog."""' + #10
  );
  try
    Assert.IsTrue(Doc.TryGetValue('key', Value));
    Assert.AreEqual('The quick brown fox jumps over the lazy dog.', Value.AsString);
  finally
    Doc.Free;
  end;
end;

procedure TTOMLMultilineStringTests.Serialize_LiteralWithSingleQuote_FallsBackToBasic;
var
  Table: TTOMLTable;
  StrVal: TTOMLString;
  Output: string;
begin
  Table := TTOMLTable.Create;
  try
    StrVal := TTOMLString.Create('it''s a test', tssLiteral);
    Table.Add('key', StrVal);
    Output := SerializeTOML(Table);
    // Should fall back to basic style because of the single quote
    Assert.Contains(Output, '"it''s a test"');
  finally
    Table.Free;
  end;
end;

procedure TTOMLMultilineStringTests.Serialize_LiteralWithoutSpecialChars_UsesLiteralStyle;
var
  Table: TTOMLTable;
  StrVal: TTOMLString;
  Output: string;
begin
  Table := TTOMLTable.Create;
  try
    StrVal := TTOMLString.Create('C:\Users\example', tssLiteral);
    Table.Add('path', StrVal);
    Output := SerializeTOML(Table);
    // Should use literal style (no escaping of backslash)
    Assert.Contains(Output, '''C:\Users\example''');
  finally
    Table.Free;
  end;
end;

procedure TTOMLMultilineStringTests.RoundTrip_BasicString_StaysBasic;
var
  Doc: TTOMLTable;
  Output: string;
begin
  Doc := ParseTOML('key = "simple value"' + #10);
  try
    Output := SerializeTOML(Doc);
    Assert.Contains(Output, '"simple value"');
    // Should NOT contain triple quotes
    Assert.AreEqual(0, Pos('"""', Output), 'Should not use multiline format');
  finally
    Doc.Free;
  end;
end;

end.
```

- [ ] **Step 2: Build to verify test unit compiles**

Run: Compile the project with the delphi-build MCP server.
Expected: Compiles successfully.

- [ ] **Step 3: Commit**

```bash
git add tests/delphi/TOML.Tests.MultilineStrings.pas
git commit -m "Add DUnitX tests for multiline string round-trip and line-ending backslash"
```

---

### Task 13: Register test units in test project

**Files:**
- Modify: `tests/delphi/TOML.Tests.dpr:8-20` (uses clause)

- [ ] **Step 1: Add new test units to the uses clause**

In `TOML.Tests.dpr`, add the new test units to the uses clause (after line 20):

```pascal
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF}
  DUnitX.TestFramework,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  TOML in '..\..\src\TOML.pas',
  TOML.Types in '..\..\src\TOML.Types.pas',
  TOML.Parser in '..\..\src\TOML.Parser.pas',
  TOML.Serializer in '..\..\src\TOML.Serializer.pas',
  TOML.Tests.NestedKeys in 'TOML.Tests.NestedKeys.pas',
  TOML.Tests.Comments in 'TOML.Tests.Comments.pas',
  TOML.Tests.MultilineStrings in 'TOML.Tests.MultilineStrings.pas';
```

- [ ] **Step 2: Build the full test project**

Run: Compile the test project with the delphi-build MCP server.
Expected: Compiles successfully.

- [ ] **Step 3: Run all tests**

Run the test executable to verify all tests pass (both new and existing).
Expected: All tests pass. If any fail, investigate and fix before proceeding.

- [ ] **Step 4: Commit**

```bash
git add tests/delphi/TOML.Tests.dpr
git commit -m "Register comment and multiline string test units in test project"
```

---

### Task 14: Fix any test failures and final verification

**Files:**
- Potentially any file from previous tasks

- [ ] **Step 1: Run all tests and capture output**

Run the compiled test executable.
Expected: All tests pass. Capture output.

- [ ] **Step 2: Fix any test failures**

If tests fail, read the failure messages, identify the root cause, and fix. Common issues:
- Body count mismatches (comments creating extra entries)
- String escaping differences in expected vs actual output
- Float formatting differences (`FloatToStr` locale sensitivity)

- [ ] **Step 3: Run existing FPC tests (if possible) to check backward compatibility**

The existing FPC tests in `tests/TestCaseTOML.pas` should still pass since `Table.Items` still works.

- [ ] **Step 4: Final commit if fixes were needed**

```bash
git add -A
git commit -m "Fix test failures from comment and multiline string integration"
```
