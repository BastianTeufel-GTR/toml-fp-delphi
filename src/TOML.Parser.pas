{ TOML Parser unit that handles parsing of TOML format data.
  This unit implements a lexer and parser for the TOML format specification.
  The parser supports all TOML data types and features including:
  - Basic key/value pairs
  - Tables and inline tables
  - Arrays
  - Basic strings and literal strings
  - Integers (decimal, hex, octal, binary)
  - Floats
  - Booleans
  - Dates and times
}
unit TOML.Parser;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, TOML.Types, Generics.Collections, TypInfo, DateUtils, Math;

type
  { Token types used during lexical analysis }
  TTokenType = (
    ttEOF,        // End of file
    ttString,     // String literal
    ttInteger,    // Integer number
    ttFloat,      // Floating point number
    ttBoolean,    // Boolean value
    ttDateTime,   // Date/time value
    ttEqual,      // = symbol
    ttDot,        // . symbol
    ttComma,      // , symbol
    ttLBracket,   // [ symbol
    ttRBracket,   // ] symbol
    ttLBrace,     // { symbol
    ttRBrace,     // } symbol
    ttNewLine,    // Line break
    ttWhitespace, // Whitespace characters
    ttComment,    // Comment
    ttIdentifier  // Identifier
  );

  { Token record that stores lexical token information }
  TToken = record
    TokenType: TTokenType;  // Type of the token
    Value: string;          // String value of the token
    Line: Integer;          // Line number where token appears
    Column: Integer;        // Column number where token appears
  end;

  { Key-Value pair type for TOML tables }
  TTOMLKeyValuePair = specialize TPair<string, TTOMLValue>;

  { Lexer class that performs lexical analysis of TOML input }
  TTOMLLexer = class
  private
    FInput: string;      // Input string to tokenize
    FPosition: Integer;  // Current position in input
    FLine: Integer;      // Current line number
    FColumn: Integer;    // Current column number
    
    { Checks if we've reached the end of input
      @returns True if at end, False otherwise }
    function IsAtEnd: Boolean;
    
    { Peeks at current character without advancing
      @returns Current character or #0 if at end }
    function Peek: Char;
    
    { Peeks at next character without advancing
      @returns Next character or #0 if at end }
    function PeekNext: Char;
    
    { Advances position and returns current character
      @returns Current character or #0 if at end }
    function Advance: Char;
    
    { Skips whitespace and comments }
    procedure SkipWhitespace;
    
    { Scans a string token (basic or literal)
      @returns The scanned string token }
    function ScanString: TToken;
    
    { Scans a number token (integer or float)
      @returns The scanned number token }
    function ScanNumber: TToken;
    
    { Scans an identifier token
      @returns The scanned identifier token }
    function ScanIdentifier: TToken;
    
    { Scans a datetime token
      @returns The scanned datetime token }
    function ScanDateTime: TToken;
    
    { Checks if character is a digit
      @param C Character to check
      @returns True if digit, False otherwise }
    function IsDigit(C: Char): Boolean;
    
    { Checks if character is alphabetic
      @param C Character to check
      @returns True if alphabetic, False otherwise }
    function IsAlpha(C: Char): Boolean;
    
    { Checks if character is alphanumeric
      @param C Character to check
      @returns True if alphanumeric, False otherwise }
    function IsAlphaNumeric(C: Char): Boolean;
  public
    { Creates a new lexer instance
      @param AInput The TOML input string to tokenize }
    constructor Create(const AInput: string);
    
    { Gets the next token from input
      @returns The next token
      @raises ETOMLParserException if invalid input encountered }
    function NextToken: TToken;
  end;

  { Parser class that performs syntactic analysis of TOML input }
  TTOMLParser = class
  private
    FLexer: TTOMLLexer;           // Lexer instance
    FCurrentToken: TToken;         // Current token being processed
    FPeekedToken: TToken;         // Next token (if peeked)
    FHasPeeked: Boolean;          // Whether we have a peeked token
    
    { Advances to next token }
    procedure Advance;
    
    { Peeks at next token without advancing
      @returns The next token }
    function Peek: TToken;
    
    { Checks if current token matches expected type
      @param TokenType Expected token type
      @returns True and advances if matches, False otherwise }
    function Match(TokenType: TTokenType): Boolean;
    
    { Expects current token to be of specific type
      @param TokenType Expected token type
      @raises ETOMLParserException if token doesn't match }
    procedure Expect(TokenType: TTokenType);
    
    { Parses a TOML value
      @returns The parsed value
      @raises ETOMLParserException on parse error }
    function ParseValue: TTOMLValue;
    
    { Parses a string value
      @returns The parsed string value }
    function ParseString: TTOMLString;
    
    { Parses a number value
      @returns The parsed number value }
    function ParseNumber: TTOMLValue;
    
    { Parses a boolean value
      @returns The parsed boolean value }
    function ParseBoolean: TTOMLBoolean;
    
    { Parses a datetime value
      @returns The parsed datetime value }
    function ParseDateTime: TTOMLDateTime;
    
    { Parses an array value
      @returns The parsed array value }
    function ParseArray: TTOMLArray;
    
    { Parses an inline table value
      @returns The parsed table value }
    function ParseInlineTable: TTOMLTable;
    
    { Parses a key
      @returns The parsed key string }
    function ParseKey: string;
    
    { Parses a key-value pair
      @returns The parsed key-value pair }
    function ParseKeyValue: TTOMLKeyValuePair;
  public
    { Creates a new parser instance
      @param AInput The TOML input string to parse }
    constructor Create(const AInput: string);
    destructor Destroy; override;
    
    { Parses the input and returns a TOML table
      @returns The parsed TOML table
      @raises ETOMLParserException on parse error }
    function Parse: TTOMLTable;
  end;

{ Helper functions }

{ Parses a TOML string into a table
  @param ATOML The TOML string to parse
  @returns The parsed TOML table
  @raises ETOMLParserException on parse error }
function ParseTOMLString(const ATOML: string): TTOMLTable;

{ Parses a TOML file into a table
  @param AFileName The file to parse
  @returns The parsed TOML table
  @raises ETOMLParserException on parse error
  @raises EFileStreamError if file cannot be opened }
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

{ TTOMLLexer implementation continues... }
// ... rest of the implementation remains unchanged

end. 
