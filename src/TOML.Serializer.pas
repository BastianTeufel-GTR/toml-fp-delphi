{ TOML.Serializer.pas
  This unit implements serialization of TOML data structures to text format.
  It handles converting TOML objects into properly formatted TOML text that follows
  the TOML v1.0.0 specification.

  The serializer supports all TOML data types and features:
  - Basic key/value pairs with proper quoting and escaping
  - Tables and inline tables with proper nesting
  - Arrays with proper formatting and type consistency
  - Basic strings and literal strings with proper escaping
  - Numbers in decimal format (integers and floats)
  - Booleans and dates/times in standard format
  
  Key features:
  - Efficient string building using TStringBuilder
  - Proper indentation and formatting for readability
  - Handles nested tables and arrays correctly
  - Preserves table ordering as per TOML spec
  - Proper escaping of special characters in strings
}
unit TOML.Serializer;

{$IF defined(FPC)}
{$mode objfpc}{$H+}{$J-}
{$ELSE}
{$H+} // Enable Long Strings
{$J-} // Disable Writeable Consts
{$ENDIF}

interface

uses
  SysUtils, Classes, TOML.Types, Generics.Collections;

type
{$IF Defined(FPC)}
  { Key-Value pair type for TOML tables }
  TTOMLKeyValuePair = specialize TPair<string, TTOMLValue>;
{$ELSE}
  { Key-Value pair type for TOML tables }
  TTOMLKeyValuePair = TPair<string, TTOMLValue>;
{$ENDIF}

  { TOML serializer class that converts TOML data to text format
    This class handles the conversion of TOML data structures into properly
    formatted TOML text, following the TOML v1.0.0 specification }
  TTOMLSerializer = class
  private
    FStringBuilder: TStringBuilder;  // StringBuilder for efficient string building
    FIndentLevel: Integer;           // Current indentation level
    FCurrentPath: TStringList;       // Tracks current table path for proper nesting
    
    { Writes indentation at current level
      Used to maintain consistent formatting }
    procedure WriteIndent;
    
    { Writes a line with optional content and newline
      @param ALine Optional string content to write }
    procedure WriteLine(const ALine: string = '');
    
    { Writes a TOML key with proper quoting
      @param AKey The key to write
      Handles escaping and quoting of keys as needed }
    procedure WriteKey(const AKey: string);
    
    { Writes a TOML string value with proper escaping
      @param AValue The string to write
      Handles all required string escaping per TOML spec }
    procedure WriteString(const AValue: string);

    { Writes a TOML string value in its original style (basic, literal, multiline)
      @param AValue The TTOMLString instance carrying both value and style
      Falls back to basic double-quoted output when the chosen style cannot
      represent the value losslessly (e.g. single quotes in a literal string) }
    procedure WriteStringValue(const AValue: TTOMLString);
    
    { Writes any TOML value based on its type
      @param AValue The value to write
      Dispatches to appropriate write method based on value type }
    procedure WriteValue(const AValue: TTOMLValue);
    
    { Writes a TOML table
      @param ATable The table to write
      @param AInline Whether to write as inline table
      Handles both standard and inline table formats }
    procedure WriteTable(const ATable: TTOMLTable; const AInline: Boolean = False);
    
    { Writes a TOML array
      @param AArray The array to write
      Handles arrays of any valid TOML type }
    procedure WriteArray(const AArray: TTOMLArray);
    
    { Writes a TOML datetime value
      @param ADateTime The datetime to write
      Formats datetime according to RFC 3339 }
    procedure WriteDateTime(const ADateTime: TDateTime);

    { Writes an inline comment for a value if one is present
      @param AValue The value whose InlineComment property is checked }
    procedure WriteInlineComment(const AValue: TTOMLValue);

    { Checks if a key needs to be quoted
      @param AKey The key to check
      @returns True if key needs quoting, False otherwise }
    function NeedsQuoting(const AKey: string): Boolean;

    { Builds a full dotted path from FCurrentPath components plus an optional key.
      @param AKey Optional key to append. When empty, builds from FCurrentPath alone.
      @returns The full dotted path with proper quoting }
    function BuildPath(const AKey: string = ''): string;
  public
    { Creates a new TOML serializer instance }
    constructor Create;
    
    { Cleans up the serializer instance }
    destructor Destroy; override;
    
    { Serializes a TOML value to string format
      @param AValue The value to serialize
      @returns The serialized TOML string
      @raises ETOMLSerializerException if value cannot be serialized }
    function Serialize(const AValue: TTOMLValue): string;
  end;

{ High-level serialization functions }

{ Serializes a TOML value to string format
  @param AValue The value to serialize
  @returns The serialized TOML string
  @raises ETOMLSerializerException if value cannot be serialized }
function SerializeTOML(const AValue: TTOMLValue): string;

{ Serializes a TOML value to a file
  @param AValue The value to serialize
  @param AFileName The output file path
  @returns True if successful, False otherwise
  @raises ETOMLSerializerException if value cannot be serialized
  @raises EFileStreamError if file cannot be written }
function SerializeTOMLToFile(const AValue: TTOMLValue; const AFileName: string): Boolean;

implementation

uses
  TOML.Parser;

{ High-level function implementations }

function SerializeTOML(const AValue: TTOMLValue): string;
var
  Serializer: TTOMLSerializer;
begin
  Serializer := TTOMLSerializer.Create;
  try
    Result := Serializer.Serialize(AValue);
  finally
    Serializer.Free;
  end;
end;

function SerializeTOMLToFile(const AValue: TTOMLValue; const AFileName: string): Boolean;
var
  TOML: string;
begin
  Result := False;
  try
    TOML := SerializeTOML(AValue);
    with TStringList.Create do
    try
      Text := TOML;
      SaveToFile(AFileName);
      Result := True;
    finally
      Free;
    end;
  except
    // Return False on any error
  end;
end;

{ TTOMLSerializer implementation }

constructor TTOMLSerializer.Create;
begin
  inherited Create;
  FStringBuilder := TStringBuilder.Create;
  FIndentLevel := 0;
  FCurrentPath := TStringList.Create;
  FCurrentPath.Delimiter := '.';      // Set delimiter for path joining
  FCurrentPath.StrictDelimiter := True; // Use strict delimiter handling
end;

destructor TTOMLSerializer.Destroy;
begin
  FStringBuilder.Free;
  FCurrentPath.Free;
  inherited;
end;

procedure TTOMLSerializer.WriteIndent;
var
  i: Integer;
begin
  for i := 1 to FIndentLevel * 2 do
    FStringBuilder.Append(' ');
end;

procedure TTOMLSerializer.WriteLine(const ALine: string = '');
begin
  if ALine <> '' then
  begin
    WriteIndent;
    FStringBuilder.Append(ALine);
  end;
  FStringBuilder.AppendLine;
end;

function TTOMLSerializer.NeedsQuoting(const AKey: string): Boolean;
var
  i: Integer;
  C: Char;
begin
  // Empty keys need quoting
  if AKey = '' then
    Exit(True);
    
  // Check all characters - must be letter, number, underscore or dash
  for i := 1 to Length(AKey) do
  begin
    C := AKey[i];
    if not ((C in ['A'..'Z']) or (C in ['a'..'z']) or 
            (C in ['0'..'9']) or (C = '_') or (C = '-')) then
      Exit(True);
  end;
  
  Result := False;
end;

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

procedure TTOMLSerializer.WriteKey(const AKey: string);
begin
  if NeedsQuoting(AKey) then
    WriteString(AKey)
  else
    FStringBuilder.Append(AKey);
end;

procedure TTOMLSerializer.WriteString(const AValue: string);
var
  i: Integer;
  C: Char;
begin
  FStringBuilder.Append('"');
  for i := 1 to Length(AValue) do
  begin
    C := AValue[i];
    case C of
      #8:  FStringBuilder.Append('\b');   // Backspace
      #9:  FStringBuilder.Append('\t');   // Tab
      #10: FStringBuilder.Append('\n');   // Line feed
      #13: FStringBuilder.Append('\r');   // Carriage return
      '"': FStringBuilder.Append('\"');   // Quote
      '\': FStringBuilder.Append('\\');   // Backslash
      else
        if C < #32 then
          // Control characters as unicode escapes
          FStringBuilder.AppendFormat('\u%.4x', [Ord(C)])
        else
          FStringBuilder.Append(C);
    end;
  end;
  FStringBuilder.Append('"');
end;

procedure TTOMLSerializer.WriteStringValue(const AValue: TTOMLString);
var
  i: Integer;
  C: Char;
  S: string;
  HasControlChars: Boolean;
begin
  S := AValue.Value;
  case AValue.Style of
    tssBasic:
      WriteString(S);

    tssLiteral:
    begin
      // Fall back to basic if value contains single quote or control chars
      HasControlChars := False;
      for i := 1 to Length(S) do
        if Ord(S[i]) < 32 then
        begin
          HasControlChars := True;
          Break;
        end;

      if (Pos('''', S) > 0) or HasControlChars then
        WriteString(S)
      else
      begin
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
          #9:  FStringBuilder.Append(#9);   // Tab allowed literally in multiline
          #10: FStringBuilder.Append(#10);  // Newline allowed literally
          #13: FStringBuilder.Append(#13);  // CR allowed literally
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
      if Pos('''''''', S) > 0 then
      begin
        // Output as multiline basic instead
        FStringBuilder.Append('"""');
        FStringBuilder.AppendLine;
        FStringBuilder.Append(EscapeTomlString(S));
        FStringBuilder.Append('"""');
      end
      else
      begin
        FStringBuilder.Append('''''''');
        FStringBuilder.AppendLine;
        FStringBuilder.Append(S);
        FStringBuilder.Append('''''''');
      end;
    end;
  end;
end;

procedure TTOMLSerializer.WriteDateTime(const ADateTime: TDateTime);
begin
  // Format as RFC 3339 UTC datetime
  FStringBuilder.Append(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', ADateTime, TOMLFormatSettings));
end;

procedure TTOMLSerializer.WriteInlineComment(const AValue: TTOMLValue);
begin
  if AValue.InlineComment <> '' then
  begin
    FStringBuilder.Append(' # ');
    FStringBuilder.Append(AValue.InlineComment);
  end;
end;

procedure TTOMLSerializer.WriteArray(const AArray: TTOMLArray);
var
  i: Integer;
  Item: TTOMLValue;
  AllTables: Boolean;
begin
  // Check if this is an array of tables (all elements are tables)
  AllTables := (AArray.Count > 0);
  for i := 0 to AArray.Count - 1 do
  begin
    if AArray.GetItem(i).ValueType <> tvtTable then
    begin
      AllTables := False;
      Break;
    end;
  end;
  
  // Arrays of tables are handled specially during top-level serialization
  if not AllTables then
  begin
    FStringBuilder.Append('[');
    
    if AArray.Count > 0 then
    begin
      for i := 0 to AArray.Count - 1 do
      begin
        if i > 0 then
          FStringBuilder.Append(', ');
        
        Item := AArray.GetItem(i);
        WriteValue(Item);
      end;
    end;
    
    FStringBuilder.Append(']');
  end;
end;

procedure TTOMLSerializer.WriteValue(const AValue: TTOMLValue);
begin
  case AValue.ValueType of
    tvtString:
      WriteStringValue(TTOMLString(AValue));
      
    tvtInteger:
      FStringBuilder.Append(IntToStr(AValue.AsInteger));
      
    tvtFloat:
      FStringBuilder.Append(FloatToStr(AValue.AsFloat, TOMLFormatSettings));
      
    tvtBoolean:
      if AValue.AsBoolean then
        FStringBuilder.Append('true')
      else
        FStringBuilder.Append('false');
        
    tvtDateTime:
      WriteDateTime(AValue.AsDateTime);
      
    tvtArray:
      WriteArray(AValue.AsArray);
      
    tvtTable, tvtInlineTable:
      WriteTable(AValue.AsTable, AValue.ValueType = tvtInlineTable);

    tvtComment:
    begin
      FStringBuilder.Append('# ');
      FStringBuilder.Append(TTOMLComment(AValue).Text);
    end;
  end;
end;

procedure TTOMLSerializer.WriteTable(const ATable: TTOMLTable; const AInline: Boolean = False);
var
  First: Boolean;
  Entry: TTOMLTableEntry;
  SubTable: TTOMLTable;
  i, j: Integer;
  ArrayValue: TTOMLArray;
  AllTables: Boolean;
begin
  if AInline then
  begin
    FStringBuilder.Append('{');
    First := True;

    for i := 0 to ATable.Body.Count - 1 do
    begin
      Entry := ATable.Body[i];
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

      if Entry.Value is TTOMLComment then
      begin
        WriteValue(Entry.Value);
        WriteLine;
        Continue;
      end;

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

function TTOMLSerializer.Serialize(const AValue: TTOMLValue): string;
begin
  FStringBuilder.Clear;
  WriteValue(AValue);
  Result := FStringBuilder.ToString;
end;

end.
