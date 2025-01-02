unit TOML.Serializer;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, TOML.Types, Generics.Collections;

type
  { Key-Value pair type }
  TTOMLKeyValuePair = specialize TPair<string, TTOMLValue>;

  TTOMLSerializer = class
  private
    FStringBuilder: TStringBuilder;
    FIndentLevel: Integer;
    FCurrentPath: TStringList;
    
    procedure WriteIndent;
    procedure WriteLine(const ALine: string = '');
    procedure WriteKey(const AKey: string);
    procedure WriteString(const AValue: string);
    procedure WriteValue(const AValue: TTOMLValue);
    procedure WriteTable(const ATable: TTOMLTable; const AInline: Boolean = False);
    procedure WriteArray(const AArray: TTOMLArray);
    procedure WriteDateTime(const ADateTime: TDateTime);
    function NeedsQuoting(const AKey: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    
    function Serialize(const AValue: TTOMLValue): string;
  end;

function SerializeTOML(const AValue: TTOMLValue): string;
function SerializeTOMLToFile(const AValue: TTOMLValue; const AFileName: string): Boolean;

implementation

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

{ TTOMLSerializer }

constructor TTOMLSerializer.Create;
begin
  inherited Create;
  FStringBuilder := TStringBuilder.Create;
  FIndentLevel := 0;
  FCurrentPath := TStringList.Create;
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
  if AKey = '' then
    Exit(True);
    
  // Check first character
  C := AKey[1];
  if not ((C in ['A'..'Z']) or (C in ['a'..'z']) or (C = '_')) then
    Exit(True);
    
  // Check rest of the characters
  for i := 2 to Length(AKey) do
  begin
    C := AKey[i];
    if not ((C in ['A'..'Z']) or (C in ['a'..'z']) or (C in ['0'..'9']) or (C = '_') or (C = '-')) then
      Exit(True);
  end;
  
  Result := False;
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
      #8:  FStringBuilder.Append('\b');
      #9:  FStringBuilder.Append('\t');
      #10: FStringBuilder.Append('\n');
      #13: FStringBuilder.Append('\r');
      '"': FStringBuilder.Append('\"');
      '\': FStringBuilder.Append('\\');
      else
        if C < #32 then
          FStringBuilder.AppendFormat('\u%.4x', [Ord(C)])
        else
          FStringBuilder.Append(C);
    end;
  end;
  FStringBuilder.Append('"');
end;

procedure TTOMLSerializer.WriteDateTime(const ADateTime: TDateTime);
begin
  FStringBuilder.Append(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', ADateTime));
end;

procedure TTOMLSerializer.WriteArray(const AArray: TTOMLArray);
var
  i: Integer;
  Item: TTOMLValue;
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

procedure TTOMLSerializer.WriteValue(const AValue: TTOMLValue);
begin
  case AValue.ValueType of
    tvtString:
      WriteString(AValue.AsString);
      
    tvtInteger:
      FStringBuilder.Append(IntToStr(AValue.AsInteger));
      
    tvtFloat:
      FStringBuilder.Append(FloatToStr(AValue.AsFloat));
      
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
  end;
end;

procedure TTOMLSerializer.WriteTable(const ATable: TTOMLTable; const AInline: Boolean = False);
var
  First: Boolean;
  Pair: TTOMLKeyValuePair;
  SubTable: TTOMLTable;
begin
  if AInline then
  begin
    FStringBuilder.Append('{');
    First := True;
    
    for Pair in ATable.Items do
    begin
      if not First then
        FStringBuilder.Append(', ')
      else
        First := False;
        
      WriteKey(Pair.Key);
      FStringBuilder.Append(' = ');
      WriteValue(Pair.Value);
    end;
    
    FStringBuilder.Append('}');
  end
  else
  begin
    // First write all non-table values
    for Pair in ATable.Items do
    begin
      if not (Pair.Value is TTOMLTable) then
      begin
        WriteKey(Pair.Key);
        FStringBuilder.Append(' = ');
        WriteValue(Pair.Value);
        WriteLine;
      end;
    end;
    
    // Then write all table values
    for Pair in ATable.Items do
    begin
      if Pair.Value is TTOMLTable then
      begin
        SubTable := TTOMLTable(Pair.Value);
        if SubTable.Items.Count > 0 then
        begin
          WriteLine;
          WriteLine('[' + Pair.Key + ']');
          WriteTable(SubTable);
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
