unit TOML.Parser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TOML.Types, Generics.Collections, TypInfo, DateUtils;

type
  { Token types for lexical analysis }
  TTokenType = (
    ttEOF,
    ttString,
    ttInteger,
    ttFloat,
    ttBoolean,
    ttDateTime,
    ttEqual,
    ttDot,
    ttComma,
    ttLBracket,
    ttRBracket,
    ttLBrace,
    ttRBrace,
    ttNewLine,
    ttWhitespace,
    ttComment,
    ttIdentifier
  );

  { Token record }
  TToken = record
    TokenType: TTokenType;
    Value: string;
    Line: Integer;
    Column: Integer;
  end;

  { Key-Value pair type }
  TTOMLKeyValuePair = specialize TPair<string, TTOMLValue>;

  { Lexer class }
  TTOMLLexer = class
  private
    FInput: string;
    FPosition: Integer;
    FLine: Integer;
    FColumn: Integer;
    function IsAtEnd: Boolean;
    function Peek: Char;
    function PeekNext: Char;
    function Advance: Char;
    procedure SkipWhitespace;
    function ScanString: TToken;
    function ScanNumber: TToken;
    function ScanIdentifier: TToken;
    function ScanDateTime: TToken;
    function IsDigit(C: Char): Boolean;
    function IsAlpha(C: Char): Boolean;
    function IsAlphaNumeric(C: Char): Boolean;
  public
    constructor Create(const AInput: string);
    function NextToken: TToken;
  end;

  { Parser class }
  TTOMLParser = class
  private
    FLexer: TTOMLLexer;
    FCurrentToken: TToken;
    FPeekedToken: TToken;
    FHasPeeked: Boolean;
    
    procedure Advance;
    function Peek: TToken;
    function Match(TokenType: TTokenType): Boolean;
    procedure Expect(TokenType: TTokenType);
    
    function ParseValue: TTOMLValue;
    function ParseString: TTOMLString;
    function ParseNumber: TTOMLValue;
    function ParseBoolean: TTOMLBoolean;
    function ParseDateTime: TTOMLDateTime;
    function ParseArray: TTOMLArray;
    function ParseInlineTable: TTOMLTable;
    function ParseKeyValue: TTOMLKeyValuePair;
    function ParseKey: string;
  public
    constructor Create(const AInput: string);
    destructor Destroy; override;
    function Parse: TTOMLTable;
  end;

  { Helper functions }
  function ParseTOMLString(const ATOML: string): TTOMLTable;
  function ParseTOMLFile(const AFileName: string): TTOMLTable;

implementation

{ Helper functions }

function ParseTOMLString(const ATOML: string): TTOMLTable;
var
  Parser: TTOMLParser;
begin
  Parser := TTOMLParser.Create(ATOML);
  try
    Result := Parser.Parse;
  finally
    Parser.Free;
  end;
end;

function ParseTOMLFile(const AFileName: string): TTOMLTable;
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    StringStream := TStringStream.Create('');
    try
      StringStream.CopyFrom(FileStream, 0);
      Result := ParseTOMLString(StringStream.DataString);
    finally
      StringStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

{ TTOMLLexer }

constructor TTOMLLexer.Create(const AInput: string);
begin
  inherited Create;
  FInput := AInput;
  FPosition := 1;
  FLine := 1;
  FColumn := 1;
end;

function TTOMLLexer.IsAtEnd: Boolean;
begin
  Result := FPosition > Length(FInput);
end;

function TTOMLLexer.Peek: Char;
begin
  if IsAtEnd then
    Result := #0
  else
    Result := FInput[FPosition];
end;

function TTOMLLexer.PeekNext: Char;
begin
  if FPosition + 1 > Length(FInput) then
    Result := #0
  else
    Result := FInput[FPosition + 1];
end;

function TTOMLLexer.Advance: Char;
begin
  if not IsAtEnd then
  begin
    Result := FInput[FPosition];
    Inc(FPosition);
    Inc(FColumn);
    if Result = #10 then
    begin
      Inc(FLine);
      FColumn := 1;
    end;
  end
  else
    Result := #0;
end;

procedure TTOMLLexer.SkipWhitespace;
begin
  while not IsAtEnd do
  begin
    case Peek of
      ' ', #9: Advance;
      '#': begin
        while (not IsAtEnd) and (Peek <> #10) do
          Advance;
      end;
      else
        Break;
    end;
  end;
end;

function TTOMLLexer.IsDigit(C: Char): Boolean;
begin
  Result := C in ['0'..'9'];
end;

function TTOMLLexer.IsAlpha(C: Char): Boolean;
begin
  Result := (C in ['a'..'z']) or (C in ['A'..'Z']) or (C = '_');
end;

function TTOMLLexer.IsAlphaNumeric(C: Char): Boolean;
begin
  Result := IsAlpha(C) or IsDigit(C);
end;

function TTOMLLexer.ScanString: TToken;
var
  IsMultiline: Boolean;
  QuoteChar: Char;
  StartColumn: Integer;
begin
  IsMultiline := False;
  StartColumn := FColumn;
  QuoteChar := Advance; // Get opening quote
  
  if (Peek = QuoteChar) and (PeekNext = QuoteChar) then
  begin
    IsMultiline := True;
    Advance; // Skip second quote
    Advance; // Skip third quote
  end;
  
  Result.Value := '';
  while not IsAtEnd do
  begin
    if IsMultiline then
    begin
      if (Peek = QuoteChar) and (PeekNext = QuoteChar) and 
         (FPosition + 2 <= Length(FInput)) and (FInput[FPosition + 2] = QuoteChar) then
      begin
        Advance; // Skip first quote
        Advance; // Skip second quote
        Advance; // Skip third quote
        Break;
      end;
    end
    else if Peek = QuoteChar then
    begin
      Advance;
      Break;
    end;
    
    if Peek = '\' then
    begin
      Advance; // Skip backslash
      case Peek of
        'n': Result.Value := Result.Value + #10;
        't': Result.Value := Result.Value + #9;
        'r': Result.Value := Result.Value + #13;
        '\': Result.Value := Result.Value + '\';
        '"': Result.Value := Result.Value + '"';
        '''': Result.Value := Result.Value + '''';
        else raise ETOMLParserException.Create('Invalid escape sequence');
      end;
      Advance;
    end
    else
      Result.Value := Result.Value + Advance;
  end;
  
  Result.TokenType := ttString;
  Result.Line := FLine;
  Result.Column := StartColumn;
end;

function TTOMLLexer.ScanNumber: TToken;
var
  IsFloat: Boolean;
  StartColumn: Integer;
begin
  IsFloat := False;
  StartColumn := FColumn;
  Result.Value := '';
  
  // Handle sign
  if Peek in ['+', '-'] then
    Result.Value := Result.Value + Advance;
    
  // Scan integer part
  while not IsAtEnd and IsDigit(Peek) do
    Result.Value := Result.Value + Advance;
    
  // Check for decimal point
  if (Peek = '.') and IsDigit(PeekNext) then
  begin
    IsFloat := True;
    Result.Value := Result.Value + Advance; // Add decimal point
    
    // Scan decimal part
    while not IsAtEnd and IsDigit(Peek) do
      Result.Value := Result.Value + Advance;
  end;
  
  // Check for exponent
  if Peek in ['e', 'E'] then
  begin
    IsFloat := True;
    Result.Value := Result.Value + Advance;
    
    if Peek in ['+', '-'] then
      Result.Value := Result.Value + Advance;
      
    while not IsAtEnd and IsDigit(Peek) do
      Result.Value := Result.Value + Advance;
  end;
  
  if IsFloat then
    Result.TokenType := ttFloat
  else
    Result.TokenType := ttInteger;
    
  Result.Line := FLine;
  Result.Column := StartColumn;
end;

function TTOMLLexer.ScanIdentifier: TToken;
var
  StartColumn: Integer;
begin
  StartColumn := FColumn;
  Result.Value := '';
  
  while not IsAtEnd and (IsAlphaNumeric(Peek) or (Peek = '-')) do
    Result.Value := Result.Value + Advance;
    
  Result.TokenType := ttIdentifier;
  Result.Line := FLine;
  Result.Column := StartColumn;
end;

function TTOMLLexer.ScanDateTime: TToken;
var
  StartColumn: Integer;
  i: Integer;
begin
  StartColumn := FColumn;
  Result.Value := '';
  
  // Scan date part (YYYY-MM-DD)
  // First scan year
  if not IsDigit(Peek) then
    Exit;
  for i := 1 to 4 do
  begin
    if not IsDigit(Peek) then
      Exit;
    Result.Value := Result.Value + Advance;
  end;
  
  // Scan month
  if Peek <> '-' then
    Exit;
  Result.Value := Result.Value + Advance;
  for i := 1 to 2 do
  begin
    if not IsDigit(Peek) then
      Exit;
    Result.Value := Result.Value + Advance;
  end;
  
  // Scan day
  if Peek <> '-' then
    Exit;
  Result.Value := Result.Value + Advance;
  for i := 1 to 2 do
  begin
    if not IsDigit(Peek) then
      Exit;
    Result.Value := Result.Value + Advance;
  end;
  
  // Check for time part
  if Peek = 'T' then
  begin
    Result.Value := Result.Value + Advance;
    
    // Scan hours
    for i := 1 to 2 do
    begin
      if not IsDigit(Peek) then
        Exit;
      Result.Value := Result.Value + Advance;
    end;
    
    // Scan minutes
    if Peek <> ':' then
      Exit;
    Result.Value := Result.Value + Advance;
    for i := 1 to 2 do
    begin
      if not IsDigit(Peek) then
        Exit;
      Result.Value := Result.Value + Advance;
    end;
    
    // Scan seconds
    if Peek <> ':' then
      Exit;
    Result.Value := Result.Value + Advance;
    for i := 1 to 2 do
    begin
      if not IsDigit(Peek) then
        Exit;
      Result.Value := Result.Value + Advance;
    end;
    
    // Check for fractional seconds
    if Peek = '.' then
    begin
      Result.Value := Result.Value + Advance;
      while IsDigit(Peek) do
        Result.Value := Result.Value + Advance;
    end;
    
    // Check for timezone
    if Peek in ['Z', '+', '-'] then
    begin
      Result.Value := Result.Value + Advance;
      if Peek <> 'Z' then
      begin
        // Hours
        for i := 1 to 2 do
        begin
          if not IsDigit(Peek) then
            Exit;
          Result.Value := Result.Value + Advance;
        end;
        
        // Optional minutes
        if Peek = ':' then
        begin
          Result.Value := Result.Value + Advance;
          for i := 1 to 2 do
          begin
            if not IsDigit(Peek) then
              Exit;
            Result.Value := Result.Value + Advance;
          end;
        end;
      end;
    end;
  end;
  
  Result.TokenType := ttDateTime;
  Result.Line := FLine;
  Result.Column := StartColumn;
end;

function TTOMLLexer.NextToken: TToken;
var
  SavePos: Integer;
  SaveLine: Integer;
  SaveCol: Integer;
begin
  SkipWhitespace;
  
  if IsAtEnd then
  begin
    Result.TokenType := ttEOF;
    Result.Value := '';
    Result.Line := FLine;
    Result.Column := FColumn;
    Exit;
  end;
  
  case Peek of
    '=': begin
      Advance;
      Result.TokenType := ttEqual;
      Result.Value := '=';
    end;
    '.': begin
      Advance;
      Result.TokenType := ttDot;
      Result.Value := '.';
    end;
    ',': begin
      Advance;
      Result.TokenType := ttComma;
      Result.Value := ',';
    end;
    '[': begin
      Advance;
      Result.TokenType := ttLBracket;
      Result.Value := '[';
    end;
    ']': begin
      Advance;
      Result.TokenType := ttRBracket;
      Result.Value := ']';
    end;
    '{': begin
      Advance;
      Result.TokenType := ttLBrace;
      Result.Value := '{';
    end;
    '}': begin
      Advance;
      Result.TokenType := ttRBrace;
      Result.Value := '}';
    end;
    #10, #13: begin
      if (Peek = #13) and (PeekNext = #10) then
        Advance; // Skip CR in CRLF
      Advance;
      Result.TokenType := ttNewLine;
      Result.Value := #10;
    end;
    '"', '''': Result := ScanString;
    '0'..'9': begin
      // Try to scan as DateTime first
      SavePos := FPosition;
      SaveLine := FLine;
      SaveCol := FColumn;
      Result := ScanDateTime;
      if Result.TokenType <> ttDateTime then
      begin
        // Restore position and scan as number
        FPosition := SavePos;
        FLine := SaveLine;
        FColumn := SaveCol;
        Result := ScanNumber;
      end;
    end;
    '+', '-': Result := ScanNumber;
    else
      if IsAlpha(Peek) then
        Result := ScanIdentifier
      else
        raise ETOMLParserException.CreateFmt('Unexpected character: %s at line %d, column %d',
          [Peek, FLine, FColumn]);
  end;
  
  Result.Line := FLine;
  Result.Column := FColumn;
end;

{ TTOMLParser }

constructor TTOMLParser.Create(const AInput: string);
begin
  inherited Create;
  FLexer := TTOMLLexer.Create(AInput);
  FHasPeeked := False;
  Advance;
end;

destructor TTOMLParser.Destroy;
begin
  FLexer.Free;
  inherited;
end;

procedure TTOMLParser.Advance;
begin
  if FHasPeeked then
  begin
    FCurrentToken := FPeekedToken;
    FHasPeeked := False;
  end
  else
    FCurrentToken := FLexer.NextToken;
end;

function TTOMLParser.Peek: TToken;
begin
  if not FHasPeeked then
  begin
    FPeekedToken := FLexer.NextToken;
    FHasPeeked := True;
  end;
  Result := FPeekedToken;
end;

function TTOMLParser.Match(TokenType: TTokenType): Boolean;
begin
  if FCurrentToken.TokenType = TokenType then
  begin
    Advance;
    Result := True;
  end
  else
    Result := False;
end;

procedure TTOMLParser.Expect(TokenType: TTokenType);
begin
  if FCurrentToken.TokenType <> TokenType then
    raise ETOMLParserException.CreateFmt('Expected token type %s but got %s at line %d, column %d',
      [GetEnumName(TypeInfo(TTokenType), Ord(TokenType)),
       GetEnumName(TypeInfo(TTokenType), Ord(FCurrentToken.TokenType)),
       FCurrentToken.Line, FCurrentToken.Column]);
  Advance;
end;

function TTOMLParser.ParseValue: TTOMLValue;
begin
  case FCurrentToken.TokenType of
    ttString: Result := ParseString;
    ttInteger, ttFloat: Result := ParseNumber;
    ttIdentifier:
      if SameText(FCurrentToken.Value, 'true') or SameText(FCurrentToken.Value, 'false') then
        Result := ParseBoolean
      else
        raise ETOMLParserException.CreateFmt('Unexpected identifier: %s at line %d, column %d',
          [FCurrentToken.Value, FCurrentToken.Line, FCurrentToken.Column]);
    ttDateTime: Result := ParseDateTime;
    ttLBracket: Result := ParseArray;
    ttLBrace: Result := ParseInlineTable;
    else
      raise ETOMLParserException.CreateFmt('Unexpected token type: %s at line %d, column %d',
        [GetEnumName(TypeInfo(TTokenType), Ord(FCurrentToken.TokenType)),
         FCurrentToken.Line, FCurrentToken.Column]);
  end;
end;

function TTOMLParser.ParseString: TTOMLString;
begin
  Result := TTOMLString.Create(FCurrentToken.Value);
  Advance;
end;

function TTOMLParser.ParseNumber: TTOMLValue;
var
  Value: string;
  Code: Integer;
  IntValue: Int64;
  FloatValue: Double;
begin
  Value := FCurrentToken.Value;
  
  if FCurrentToken.TokenType = ttInteger then
  begin
    Val(Value, IntValue, Code);
    if Code = 0 then
      Result := TTOMLInteger.Create(IntValue)
    else
      raise ETOMLParserException.CreateFmt('Invalid integer value: %s at line %d, column %d',
        [Value, FCurrentToken.Line, FCurrentToken.Column]);
  end
  else
  begin
    Val(Value, FloatValue, Code);
    if Code = 0 then
      Result := TTOMLFloat.Create(FloatValue)
    else
      raise ETOMLParserException.CreateFmt('Invalid float value: %s at line %d, column %d',
        [Value, FCurrentToken.Line, FCurrentToken.Column]);
  end;
  
  Advance;
end;

function TTOMLParser.ParseBoolean: TTOMLBoolean;
begin
  Result := TTOMLBoolean.Create(SameText(FCurrentToken.Value, 'true'));
  Advance;
end;

function TTOMLParser.ParseDateTime: TTOMLDateTime;
var
  DateStr: string;
  DateTime: TDateTime;
begin
  DateStr := FCurrentToken.Value;
  try
    DateTime := ISO8601ToDate(DateStr);
    Result := TTOMLDateTime.Create(DateTime);
  except
    on E: Exception do
      raise ETOMLParserException.CreateFmt('Invalid datetime value: %s at line %d, column %d',
        [DateStr, FCurrentToken.Line, FCurrentToken.Column]);
  end;
  Advance;
end;

function TTOMLParser.ParseArray: TTOMLArray;
begin
  Result := TTOMLArray.Create;
  try
    Expect(ttLBracket);
    
    if FCurrentToken.TokenType <> ttRBracket then
    begin
      repeat
        Result.Add(ParseValue);
      until not Match(ttComma);
    end;
    
    Expect(ttRBracket);
  except
    Result.Free;
    raise;
  end;
end;

function TTOMLParser.ParseInlineTable: TTOMLTable;
begin
  Result := TTOMLTable.Create;
  try
    Expect(ttLBrace);
    
    if FCurrentToken.TokenType <> ttRBrace then
    begin
      repeat
        with ParseKeyValue do
          Result.Add(Key, Value);
      until not Match(ttComma);
    end;
    
    Expect(ttRBrace);
  except
    Result.Free;
    raise;
  end;
end;

function TTOMLParser.ParseKey: string;
begin
  if FCurrentToken.TokenType = ttString then
  begin
    Result := FCurrentToken.Value;
    Advance;
  end
  else if FCurrentToken.TokenType = ttIdentifier then
  begin
    Result := FCurrentToken.Value;
    Advance;
  end
  else
    raise ETOMLParserException.CreateFmt('Expected string or identifier but got %s at line %d, column %d',
      [GetEnumName(TypeInfo(TTokenType), Ord(FCurrentToken.TokenType)),
       FCurrentToken.Line, FCurrentToken.Column]);
end;

function TTOMLParser.ParseKeyValue: TTOMLKeyValuePair;
var
  Key: string;
begin
  Key := ParseKey;
  
  while Match(ttDot) do
    Key := Key + '.' + ParseKey;
    
  Expect(ttEqual);
  
  Result := TTOMLKeyValuePair.Create(Key, ParseValue);
end;

function TTOMLParser.Parse: TTOMLTable;
var
  CurrentTable: TTOMLTable;
  TablePath: TStringList;
  i: Integer;
  Key: string;
  Value: TTOMLValue;
  KeyPair: TTOMLKeyValuePair;
begin
  Result := TTOMLTable.Create;
  try
    CurrentTable := Result;
    TablePath := TStringList.Create;
    try
      while FCurrentToken.TokenType <> ttEOF do
      begin
        case FCurrentToken.TokenType of
          ttLBracket:
          begin
            Advance;
            TablePath.Clear;
            
            repeat
              TablePath.Add(ParseKey);
            until not Match(ttDot);
            
            Expect(ttRBracket);
            
            // Navigate to the correct table
            CurrentTable := Result;
            for i := 0 to TablePath.Count - 2 do
            begin
              Key := TablePath[i];
              if not CurrentTable.TryGetValue(Key, Value) then
              begin
                Value := TTOMLTable.Create;
                CurrentTable.Add(Key, Value);
              end;
              if not (Value is TTOMLTable) then
                raise ETOMLParserException.CreateFmt('Key %s is not a table at line %d, column %d',
                  [Key, FCurrentToken.Line, FCurrentToken.Column]);
              CurrentTable := TTOMLTable(Value);
            end;
            
            // Create the new table
            Key := TablePath[TablePath.Count - 1];
            if not CurrentTable.TryGetValue(Key, Value) then
            begin
              Value := TTOMLTable.Create;
              CurrentTable.Add(Key, Value);
            end;
            if not (Value is TTOMLTable) then
              raise ETOMLParserException.CreateFmt('Key %s is not a table at line %d, column %d',
                [Key, FCurrentToken.Line, FCurrentToken.Column]);
            CurrentTable := TTOMLTable(Value);
          end;
          
          ttIdentifier, ttString:
          begin
            KeyPair := ParseKeyValue;
            CurrentTable.Add(KeyPair.Key, KeyPair.Value);
          end;
          
          ttNewLine: Advance;
          
          else
            raise ETOMLParserException.CreateFmt('Unexpected token type: %s at line %d, column %d',
              [GetEnumName(TypeInfo(TTokenType), Ord(FCurrentToken.TokenType)),
               FCurrentToken.Line, FCurrentToken.Column]);
        end;
      end;
    finally
      TablePath.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end. 
